use anyhow::{anyhow, Error};
use edit_distance::edit_distance;
use git2::{Blob, Oid};
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::{HashMap, BTreeSet, VecDeque};
use std::ffi::OsStr;
use std::num::NonZeroUsize;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::str::from_utf8;
use substring::Substring;

use crate::common::{BlobFile, File, Location, Name, NonWhitespaceLine};
use crate::errors::{ParseError, ParseErrorKind};
use crate::message::Ignore;

lazy_static! {
    static ref WHITESPACE_REGEX: Regex = Regex::new(r"^\s*$").unwrap();

    // This purposefully includes extra characters so that we can catch common misspellings.
    static ref UNWRAPPING_REGEX: Regex =
        Regex::new(r####"(?P<bang>!*)(?P<gap0>\s*)(?P<open>\[+)(?P<gap1>\s*)(?P<keyword>\w{12,}\.)(?P<decl>.*)(?P<close>\]{2,})(?P<extra>.*)$"####).unwrap();

    // TODO: we probably want some external source (or maybe even a config you can pass in?) for the
    // comment mapping, but we can use this for now.
    static ref EXTENSION_TO_COMMENT_REGEX: HashMap<&'static OsStr, Regex> = HashMap::from([
        (OsStr::new("rs"), Regex::new(r"^\s*//\s*(?P<text>.*)$").unwrap()),
        (OsStr::new("py"), Regex::new(r"^\s*#\s*(?P<text>.*)$").unwrap()),
        (OsStr::new("md"), Regex::new(r"^\s*<!--\s*(?P<text>.*?)\s*-->$").unwrap()),
    ]);
}

/// Node storing the information in a single `![[ChangeTogether.With(...)]]` declaration.
#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub(crate) struct ParsedLink<'a> {
    pub file: File<'a>,
    pub tag: Option<Name<'a>>,
}

// A `ParsedSpec` defines a single `Start...End` block. All names and line numbers are relative to
/// the new state, not that of the commit being diffed against.
#[derive(Debug)]
pub(crate) struct ParsedSpec<'a> {
    pub file: PathBuf,
    pub tag: Option<Name<'a>>,
    pub block: Range<NonZeroUsize>,
    pub content: Range<NonZeroUsize>,
    pub links: BTreeSet<ParsedLink<'a>>,
}

#[derive(Debug)]
pub(crate) struct Parser<'a> {
    blob_files: &'a HashMap<Oid, BlobFile<'a>>,
    errs: Vec<ParseError<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(blob_files: &'a HashMap<Oid, BlobFile<'a>>) -> Parser<'a> {
        Parser {
            blob_files,
            errs: vec![],
        }
    }

    /// Parse all of the file `blobs` supplied to this parser.
    pub fn parse(
        mut self,
        ignoring: &'a BTreeSet<Ignore<'a>>,
    ) -> Result<Vec<ParsedSpec<'a>>, Vec<ParseError<'a>>> {
        // Clear errors, just in case the `Parser` is accidentally re-used.
        self.errs = vec![];

        // Record the first line that results in an error. This will prevent us from proceeding to
        // the next step.
        let mut fatal_err: Result<(), Error> = Ok(());

        let parsed_specs = self
            .blob_files
            .into_iter()
            .fold(vec![], |mut acc, (_, blob_file)| {
                // TODO: maybe remove clone?
                match self.parse_blob_file(blob_file.path.clone(), &blob_file.blob, ignoring) {
                    Ok(mut parsed_specs) => {
                        acc.append(&mut parsed_specs);
                    }
                    Err(err) => {
                        // Record the failed compilation, but keep going for other blobs to recover
                        // more errors.
                        fatal_err = Err(err);
                    }
                };
                acc
            });

        // If even a single compilation failed in a non-recoverable way, we do not proceed.
        if let Err(_) = fatal_err {
            return Err(self.errs);
        }
        Ok(parsed_specs)
    }

    fn parse_blob_file(
        &mut self,
        path: PathBuf,
        blob: &'a Blob<'a>,
        ignoring: &'a BTreeSet<Ignore<'a>>,
    ) -> Result<Vec<ParsedSpec<'a>>, Error> {
        // Short-circuit: if the user wants to ignore everything, just return an empty vector.
        if ignoring.contains(&Ignore::All) {
            return Ok(vec![]);
        }

        // TODO: actually use `ignoring` to filter out files from parsing.

        let utf8 = match from_utf8(blob.content()) {
            Ok(utf8) => utf8,
            // Non-UTF8 files are okay to have in the diff, but we ignore them for analysis
            // purposes.
            Err(_) => return Ok(vec![]),
        };

        // Ignore files with unknown extensions.
        // TODO: clean up this nesting.
        let ext_regex = match path.extension() {
            Some(ext) => match EXTENSION_TO_COMMENT_REGEX.get(ext) {
                Some(regex) => regex,
                // Unknown extension.
                None => return Ok(vec![]),
            },
            // No extension.
            None => return Ok(vec![]),
        };

        println!("check: {:#?}", ext_regex);
        // The basic unit of parsing is the line.
        let decls = utf8
            .split(|ch| ch == '\n')
            .enumerate()
            .filter_map(|(index, line)| {
                match WHITESPACE_REGEX.is_match(line) {
                    true => None,
                    false => Some(NonWhitespaceLine {
                        // Line numbers are 1-indexed. Unwrapping is safe because we are literally
                        // adding 1 to the usize right here.
                        num: (index + 1).try_into().unwrap(),
                        text: line.strip_suffix("\r\n").unwrap_or(line), // TODO: remove unwrap
                    }),
                }
            })
            .try_fold(vec![], |mut acc, line| {
                println!("parsing: {:#?}", line);
                match self.parse_line(line, ext_regex) {
                    Ok(decl_text) => match decl_text {
                        None => {}
                        Some(decl) => {
                            println!("decl: {:#?}", decl);
                            acc.push(decl);
                        }
                    },
                    Err(e) => return Err(e),
                };
                Ok(acc)
            })?;

        self.compile_file(path, decls)
    }

    /// Take a list of `Decl`s in a given file, and compose them into a list of compiled
    /// `ParsedSpec`s.
    fn compile_file(
        &mut self,
        // TODO: we should add the file path to the `Location`, rather than piping it through here.
        path: PathBuf,
        decls: Vec<Decl<'a>>,
    ) -> Result<Vec<ParsedSpec<'a>>, Error> {
        let mut specs = vec![];
        let mut builder = None;
        println!("compiling decls: {:#?}", decls);

        for decl in decls {
            match decl.data {
                DeclData::Start(maybe_name) => match builder {
                    Some(_) => {
                        self.errs.push(ParseError::new(
                            ParseErrorKind::UnexpectedStartDecl,
                            decl.loc,
                        ));
                        return Err(anyhow!("unexpected start decl"));
                    }
                    None => {
                        builder =
                            Some(ParsedSpecBuilder::start(path.clone(), maybe_name, decl.loc));
                    }
                },
                DeclData::With(file, maybe_tag) => match &mut builder {
                    Some(builder) => builder.with(file, maybe_tag, decl.loc),
                    None => {
                        self.errs.push(ParseError::new(
                            ParseErrorKind::UnexpectedWithDecl,
                            decl.loc,
                        ));
                        return Err(anyhow!("unexpected with decl"));
                    }
                },
                DeclData::End(maybe_name) => match builder {
                    Some(b) => {
                        match b.end(maybe_name, decl.loc) {
                            Ok(spec) => specs.push(spec),
                            Err(errs) => self.errs.push(errs),
                        }
                        builder = None
                    }
                    None => {
                        self.errs
                            .push(ParseError::new(ParseErrorKind::UnexpectedEndDecl, decl.loc));
                        return Err(anyhow!("unexpected end decl"));
                    }
                },
            }
        }

        Ok(specs)
    }

    // TODO: our parsing strategy for this is pretty wasteful, since we have to read the line twice
    // via a regex before we even start parsing the annotation itself. We may want to look at doing
    // more traditional lexing with a single pass to speed things up in the future.
    fn parse_line<'b>(
        &mut self,
        line: NonWhitespaceLine<'a>,
        comment_regex: &'b Regex,
    ) -> Result<Option<Decl<'a>>, Error> {
        // See if we can get a matching comment line structure inside the brackets. If we can't, we
        // assume that this was not a correct comment line. Note that the `UNWRAPPING_REGEX` is
        // pretty forgiving of errors, so we can catch some obvious misspellings here.
        let parsable_segment = match self.unwrap_line_contents(line, comment_regex)? {
            Some(unbracketed_text) => unbracketed_text,
            None => return Ok(None),
        };
        println!("segment: {:#?}", parsable_segment);

        match self.parse_decl(line, parsable_segment) {
            Ok(decl) => Ok(Some(decl)),
            Err(err) => Err(err),
        }
    }

    /// Is this even a comment line? Does it have content? Does it have the correct brackets? Three
    /// possible answers:
    ///
    /// 1. This is not a comment line, or at least not one we think is targeted at `ChangeTogether`,
    ///    return `Ok(None)`.
    /// 2. This is an invalidly spelled `ChangeTogether` line, return an `Err(...)`.
    /// 3. This is a valid `ChangeTogether` line, in which case we return `Ok(Some(...))`.
    fn unwrap_line_contents<'b>(
        &mut self,
        line: NonWhitespaceLine<'a>,
        comment_regex: &'b Regex,
    ) -> Result<Option<ParsableLineSegment<'a>>, Error> {
        let comment = match comment_regex.captures(line.text) {
            None => return Ok(None),
            Some(matched) => matched,
        };
        let comment_text = match comment.name("text") {
            None => return Ok(None),
            Some(text) => text,
        };
        let comment_text_str = comment_text.as_str();
        let comment_text_offset = comment_text.start();
        println!("comment_text: {:#?}", comment_text_str);

        // This is definitely a non-empty comment line, let's see if it is a `ChangeTogether` line,
        // or a close misspelling of one.
        let matches = match UNWRAPPING_REGEX.captures(comment_text_str) {
            Some(matches) => matches,
            None => return Ok(None),
        };

        // The unwraps below are safe, because we want to always panic if one of these named groups
        // was not found in our regex, as this is a programmer error.
        let bangs = matches.name("bang").unwrap();
        let gap0 = matches.name("gap0").unwrap();
        let open = matches.name("open").unwrap();
        let gap1 = matches.name("gap1").unwrap();
        let keyword = matches.name("keyword").unwrap();
        let decl = matches.name("decl").unwrap();
        let close = matches.name("close").unwrap();
        let extra = matches.name("extra").unwrap();

        // Look for close misspellings of `ChangeTogether`.
        let dist = edit_distance(keyword.as_str(), "ChangeTogether.");
        if dist > 3 {
            // If the distance is too great, just assume its something else altogether.
            return Ok(None);
        } else if dist > 0 {
            // Misspelling!
            self.errs.push(ParseError::new(
                ParseErrorKind::SpellingKeyword {
                    keyword: keyword.as_str().to_string(),
                },
                Location::new(line, keyword.start(), keyword.len()),
            ));
        }

        // TODO: this repetition below is painful, clean this up.
        if bangs.len() != 1 {
            self.errs.push(ParseError::new(
                ParseErrorKind::SpellingLeadingBangs {
                    counted: bangs.len(),
                },
                Location::new(line, bangs.start(), bangs.len()),
            ));
        }
        if gap0.len() != 0 {
            self.errs.push(ParseError::new(
                ParseErrorKind::SpellingSpacesAfterBang {
                    counted: gap0.len(),
                },
                Location::new(line, gap0.start(), gap0.len()),
            ));
        }
        if open.len() != 2 {
            self.errs.push(ParseError::new(
                ParseErrorKind::SpellingOpenBrackets {
                    counted: open.len(),
                },
                Location::new(line, open.start(), open.len()),
            ));
        }
        if gap1.len() != 0 {
            self.errs.push(ParseError::new(
                ParseErrorKind::SpellingSpacesAfterOpenBrackets {
                    counted: gap1.len(),
                },
                Location::new(line, gap1.start(), gap1.len()),
            ));
        }
        if close.len() != 2 {
            self.errs.push(ParseError::new(
                ParseErrorKind::SpellingCloseBrackets {
                    counted: close.len(),
                },
                Location::new(line, close.start(), close.len()),
            ));
        }
        if extra.len() != 0 {
            let ex = extra.as_str();
            if !ex.chars().all(|ch| ch.is_whitespace()) {
                self.errs.push(ParseError::new(
                    ParseErrorKind::SpellingExtra {
                        extra: ex.to_string(),
                    },
                    Location::new(line, close.start(), close.len()),
                ));
            }
        }

        Ok(Some(ParsableLineSegment {
            text: decl.as_str(),
            pos: decl.start() + comment_text_offset,
        }))
    }

    /// The parser for the parsable segment of a single `![[ChangeTogether...]]` declaration.
    fn parse_decl(
        &mut self,
        line: NonWhitespaceLine<'a>,
        data: ParsableLineSegment<'a>,
    ) -> Result<Decl<'a>, Error> {
        println!("lexing: {:#?}", data);
        let mut tokens = self.lex_decl(data.text, line, data.pos)?;
        println!("tokens: {:#?}", tokens);
        let method = match tokens.pop_front() {
            None => {
                self.errs.push(ParseError::new(
                    ParseErrorKind::EmptyDecl,
                    Location::new(line, data.pos, 1),
                ));
                return Err(anyhow!("empty decl"));
            }
            Some(method) => method,
        };

        match method.data {
            TokenData::Identifier(word) => {
                // Check for correctly spelled method names.
                if word == "Start" {
                    return self.parse_scope_decl(method, true, tokens);
                } else if word == "With" {
                    return self.parse_with_decl(method, tokens);
                } else if word == "End" {
                    return self.parse_scope_decl(method, false, tokens);
                }

                // Check for common misspellings, record the error and continue.
                if edit_distance(word, "Start") < 2 {
                    self.errs.push(ParseError::new(
                        ParseErrorKind::UnknownMethodSuggest {
                            word: word.into(),
                            suggest: "Start".into(),
                        },
                        method.loc,
                    ));
                    return self.parse_scope_decl(method, true, tokens);
                } else if edit_distance(word, "With") < 2 {
                    self.errs.push(ParseError::new(
                        ParseErrorKind::UnknownMethodSuggest {
                            word: word.into(),
                            suggest: "With".into(),
                        },
                        method.loc,
                    ));
                    return self.parse_with_decl(method, tokens);
                } else if edit_distance(word, "End") < 2 {
                    self.errs.push(ParseError::new(
                        ParseErrorKind::UnknownMethodSuggest {
                            word: word.into(),
                            suggest: "End".into(),
                        },
                        method.loc,
                    ));
                    return self.parse_scope_decl(method, false, tokens);
                }

                // We have no idea what the user wrote.
                self.errs.push(ParseError::new(
                    ParseErrorKind::UnknownMethod { word: word.into() },
                    method.loc,
                ));
                Err(anyhow!("method unrecognized"))
            }
            _ => {
                self.errs
                    .push(ParseError::new(ParseErrorKind::UnexpectedToken, method.loc));
                Err(anyhow!("unexpected token"))
            }
        }
    }

    /// The lexer for the parsable segment of a single `ChangeTogether` line.
    fn lex_decl(
        &mut self,
        text: &'a str,
        line: NonWhitespaceLine<'a>,
        offset: usize,
        // TODO: maybe return iterator instead of vec?
    ) -> Result<VecDeque<Token<'a>>, Error> {
        let mut tokens = VecDeque::new();
        let mut from = 0;
        let mut in_string = false;
        for (i, ch) in text.chars().enumerate() {
            println!("for char: {:#?}, text: {}", ch, text);
            // Is this the end of a string?
            if in_string {
                println!("in_string: {:#?}, from: {}, tokens: {}", ch, from, tokens.len());
                if ch == '"' {
                    in_string = false;
                    tokens.push_back(Token {
                        loc: Location::new(line, from + offset, i - from),
                        data: TokenData::StringContents(text.substring(from, i)),
                    });
                    from = i + 1
                }
                continue;
            }

            // Is it the start of a string?
            if ch == '"' {
                in_string = true;
                from = i + 1;
                continue;
            }

            // Is it one of the single-char tokens?
            let mut found_token = None;
            if ch == '(' {
                found_token = Some(TokenData::OpenParen);
            } else if ch == ',' {
                found_token = Some(TokenData::Comma);
            } else if ch == ')' {
                found_token = Some(TokenData::CloseParen);
            }
            if let Some(token_data) = found_token {
                println!("found token : {:#?}", &token_data);
                if from < i {
                    tokens.push_back(Token {
                        loc: Location::new(line, from + offset, i - from),
                        data: TokenData::Identifier(text.substring(from, i)),
                    });
                }
                tokens.push_back(Token {
                    loc: Location::new(line, from + offset, 1),
                    data: token_data,
                });
                from = i + 1;
                println!("after found_token: {:#?}, from: {}", tokens, from);
                continue;
            }

            // Ignore whitespace, but use it to demarcate the end of a token if necessary.
            if ch == ' ' || ch == '\t' {
                if from < i {
                    tokens.push_back(Token {
                        loc: Location::new(line, from + offset, i - from),
                        data: TokenData::Identifier(text.substring(from, i)),
                    });
                }
                from = i + 1;
            }

            // Only (valid) option left: we are inside an identifier.
            if ch.is_alphanumeric() {
                println!("for letter: {:#?}, from: {}", ch, from);
                continue;
            }

            self.errs.push(ParseError::new(
                ParseErrorKind::InvalidToken,
                Location::new(line, i, 1),
            ));
            return Err(anyhow!("lexing failed"));
        }

        // Close any trailing identifiers.
        if from < text.len() {
            tokens.push_back(Token {
                loc: Location::new(line, from + offset, text.len() - from),
                data: TokenData::Identifier(text.substring(from, text.len())),
            });
        }

        Ok(tokens)
    }

    /// Parse a `Start` or `End` decl. These both have identical grammar, so we can parse them using
    /// the same function, with a simple boolean differentiate which of the two declarations we're
    /// building. Because the scope name argument is optional, these declarations can take one of
    /// two forms (examples below are for `Start`, but would be identical for `End`):
    ///  
    /// 1. `Start`
    /// 2. `Start(myRegion)`
    ///
    /// Note that the front `With` token has already been processed by the time we enter this
    /// function, so we just need to handle the parentheses and whatever is contained within.
    fn parse_scope_decl(
        &mut self,
        method_token: Token<'a>,
        // TODO: should probably add `Start`, `End`, and `With` token kinds instead of this bool.
        is_start: bool,
        mut tokens: VecDeque<Token<'a>>,
    ) -> Result<Decl<'a>, Error> {
        // Since we have already parsed the method name token, it is possible for there to be no
        // tokens left if we have a no-argument declaration. If so, we can just exit right here.
        let mut loc = match tokens.pop_front() {
            None => {
                return Ok(Decl::new(
                    match is_start {
                        true => DeclData::Start(None),
                        false => DeclData::End(None),
                    },
                    method_token.loc,
                ))
            }
            Some(token) => match token.data {
                TokenData::OpenParen => method_token.loc.concat(token.loc),
                _ => {
                    self.errs
                        .push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                    return Err(anyhow!("unexpected token"));
                }
            },
        };

        // We are looking at the one-argument variant - parse the name.
        let name = match tokens.pop_front() {
            None => {
                self.errs
                    .push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
                return Err(anyhow!("unclosed paren"));
            }
            Some(token) => match token.data {
                TokenData::Identifier(contents) => Name::new(contents, token.loc),
                TokenData::StringContents(contents) => {
                    self.errs.push(ParseError::new(
                        ParseErrorKind::UnnecessaryScopeString,
                        token.loc,
                    ));
                    Name::new(contents, token.loc)
                }
                TokenData::CloseParen => {
                    self.errs.push(ParseError::new(
                        ParseErrorKind::UnnecessaryParens,
                        token.loc,
                    ));
                    return Ok(Decl::new(
                        match is_start {
                            true => DeclData::Start(None),
                            false => DeclData::End(None),
                        },
                        loc.concat(token.loc),
                    ));
                }
                _ => {
                    self.errs
                        .push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                    return Err(anyhow!("unexpected token"));
                }
            },
        };

        // If we've parsed the name successfully must be a closing paren.
        loc.concat(name.loc);
        loc = match tokens.pop_front() {
            None => {
                self.errs
                    .push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
                return Err(anyhow!("unclosed paren"));
            }
            Some(token) => match token.data {
                TokenData::CloseParen => loc.concat(token.loc),
                _ => {
                    self.errs
                        .push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                    return Err(anyhow!("unexpected token"));
                }
            },
        };

        // Make sure there are no extra tokens after the end.
        match tokens.pop_front() {
            Some(token) => {
                self.errs
                    .push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                Err(anyhow!("unexpected token"))
            }
            None => Ok(Decl::new(
                match is_start {
                    true => DeclData::Start(Some(name)),
                    false => DeclData::End(Some(name)),
                },
                loc,
            )),
        }
    }

    /// Parse a `With` decl. These optionally have a second tag argument, and the first file path
    /// argument may optionally be quoted (to allow most weird characters in paths), so we are
    /// parsing 4 kinds of declarations:
    ///
    /// 1. `With(/my/fun/file.ext)`
    /// 2. `With("/my/fun/file.ext")`
    /// 3. `With(/my/fun/file.ext, someTag)`
    /// 4. `With("/my/fun/file.ext", someTag)`
    ///
    /// Note that the front `With` token has already been processed by the time we enter this
    /// function, so we just need to handle the parentheses and whatever is contained within.
    fn parse_with_decl(
        &mut self,
        method_token: Token<'a>,
        mut tokens: VecDeque<Token<'a>>,
    ) -> Result<Decl<'a>, Error> {
        let mut has_second_arg = false;
        let mut loc = match tokens.pop_front() {
            None => {
                self.errs.push(ParseError::new(
                    ParseErrorKind::EmptyWithDecl,
                    method_token.loc,
                ));
                return Err(anyhow!("empty with decl"));
            }
            Some(token) => match token.data {
                TokenData::OpenParen => method_token.loc.concat(token.loc),
                _ => {
                    self.errs
                        .push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                    return Err(anyhow!("unexpected token"));
                }
            },
        };

        let file = match tokens.pop_front() {
            None => {
                self.errs
                    .push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
                return Err(anyhow!("unclosed paren"));
            }
            Some(token) => match token.data {
                TokenData::CloseParen => {
                    self.errs.push(ParseError::new(
                        ParseErrorKind::EmptyWithDecl,
                        loc.concat(token.loc),
                    ));
                    return Err(anyhow!("unexpected token"));
                }
                // File paths may be optionally quoted, so either an `Identifier` or a
                // `StringContents` is acceptable here.
                TokenData::Identifier(contents) | TokenData::StringContents(contents) => {
                    File::new(Path::new(contents), token.loc)
                }
                _ => {
                    self.errs
                        .push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                    return Err(anyhow!("unexpected token"));
                }
            },
        };

        // Parse the token after the mandatory file path. If it's a comma, assume we are parsing a
        // two-argument variant. If its a closing paren, assume it's the one-argument variant
        // instead.
        loc.concat(file.loc);
        loc = match tokens.pop_front() {
            None => {
                self.errs
                    .push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
                return Err(anyhow!("unclosed paren"));
            }
            Some(token) => match token.data {
                TokenData::CloseParen => {
                    has_second_arg = true;
                    loc.concat(token.loc)
                }
                TokenData::Comma => loc.concat(token.loc),
                _ => {
                    self.errs
                        .push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                    return Err(anyhow!("unexpected token"));
                }
            },
        };

        // If the last token was not comma, assume this is the one-argument variant.
        if has_second_arg {
            // Make sure there are no extra tokens after the end.
            return match tokens.pop_front() {
                Some(token) => {
                    self.errs
                        .push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                    Err(anyhow!("unexpected token"))
                }
                None => Ok(Decl::new(DeclData::With(file, None), loc)),
            };
        }

        // Parse the second tag name argument.
        let name = match tokens.pop_front() {
            None => {
                self.errs
                    .push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
                return Err(anyhow!("unclosed paren"));
            }
            Some(token) => match token.data {
                TokenData::Identifier(contents) => Name::new(contents, token.loc),
                TokenData::StringContents(contents) => {
                    self.errs.push(ParseError::new(
                        ParseErrorKind::UnnecessaryScopeString,
                        token.loc,
                    ));
                    Name::new(contents, token.loc)
                }
                TokenData::CloseParen => {
                    self.errs
                        .push(ParseError::new(ParseErrorKind::UnnecessaryComa, token.loc));
                    return Ok(Decl::new(DeclData::Start(None), loc.concat(token.loc)));
                }
                _ => {
                    self.errs
                        .push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                    return Err(anyhow!("unexpected token"));
                }
            },
        };

        // The next token must be the closing paren.
        loc.concat(name.loc);
        loc = match tokens.pop_front() {
            None => {
                self.errs
                    .push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
                return Err(anyhow!("unclosed paren"));
            }
            Some(token) => match token.data {
                TokenData::CloseParen => loc.concat(token.loc),
                _ => {
                    self.errs
                        .push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                    return Err(anyhow!("unexpected token"));
                }
            },
        };

        // Make sure there are no extra tokens after the end.
        return match tokens.pop_front() {
            Some(token) => {
                self.errs
                    .push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                Err(anyhow!("unexpected token"))
            }
            None => Ok(Decl::new(DeclData::With(file, Some(name)), loc)),
        };
    }
}

/// A `ParsedSpec` defines a single `Start...End` block. All names and line numbers are relative to
/// the new state, not that of the commit being diffed against.
struct ParsedSpecBuilder<'a> {
    pub file: PathBuf,
    pub tag: Option<Name<'a>>,
    pub block_start_line: NonZeroUsize,
    pub last_decl_line: NonZeroUsize,
    pub links: BTreeSet<ParsedLink<'a>>,
}

impl<'a> ParsedSpecBuilder<'a> {
    pub fn start(file: PathBuf, tag: Option<Name<'a>>, loc: Location<'a>) -> ParsedSpecBuilder<'a> {
        ParsedSpecBuilder {
            file,
            tag,
            block_start_line: loc.line.num,
            last_decl_line: loc.line.num,
            links: BTreeSet::new(),
        }
    }

    pub fn with(&mut self, file: File<'a>, tag: Option<Name<'a>>, loc: Location<'a>) {
        self.links.insert(ParsedLink { file, tag });
        self.last_decl_line = loc.line.num;
    }

    pub fn end(
        self,
        tag: Option<Name<'a>>,
        loc: Location<'a>,
    ) -> Result<ParsedSpec<'a>, ParseError<'a>> {
        // TODO: check that tags on start and end match.
        // TODO: check that content portion is not empty.
        Ok(ParsedSpec {
            file: self.file,
            tag: self.tag,
            block: self.block_start_line..(loc.line.num.checked_add(1).unwrap()),
            content: (self.last_decl_line.checked_add(1).unwrap())..loc.line.num,
            links: self.links,
        })
    }
}

/// The "unwrapped" portion of a single comment line. For example, in a Rust file, the `-` parts
/// below are the "wrapper" (ie, a common set of marking characters identifying a line relevant to
/// this application), while the `^` parts are the parsable segment, which contains a single
/// statement relevant to `ChangeTogether`:
///
///   // ![[ChangeTogether.Start(myCoolTag)]]
///   ---------------------^^^^^^^^^^^^^^^^--
///
#[derive(Copy, Clone, Debug)]
struct ParsableLineSegment<'a> {
    pub text: &'a str,
    pub pos: usize,
}

/// A declaration (ie, the variable part of the `ChangeTogether` statement) and its source line,
/// useful together for error reporting.
#[derive(Debug)]
struct Decl<'a> {
    /// Refers to the position of the `.` in the `line`.
    pub loc: Location<'a>,
    pub data: DeclData<'a>,
}

impl<'a> Decl<'a> {
    fn new(data: DeclData<'a>, loc: Location<'a>) -> Decl<'a> {
        Decl { data, loc }
    }
}

#[derive(Debug)]
enum DeclData<'a> {
    Start(Option<Name<'a>>),
    With(File<'a>, Option<Name<'a>>),
    End(Option<Name<'a>>),
}

/// A single discrete grammatical element inside the parsable segment of a line.
#[derive(Debug)]
struct Token<'a> {
    pub loc: Location<'a>,
    pub data: TokenData<'a>,
}

#[derive(Debug)]
enum TokenData<'a> {
    Identifier(&'a str),
    OpenParen,
    StringContents(&'a str),
    CloseParen,
    Comma,
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use git2::Repository;
    use std::collections::BTreeSet;
    use std::num::NonZeroUsize;
    use std::{collections::HashMap, error::Error as Err};
    use tempfile::TempDir;

    use crate::common::BlobFile;
    use crate::errors::ParseError;
    use crate::parser::ParsedLink;
    use crate::testing::helpers::create_test_file_blob;

    use super::{ParsedSpec, Parser};

    const FILE_EXT_MD: &'static str = "test.md";
    const FILE_EXT_PY: &'static str = "test.py";
    const FILE_EXT_RS: &'static str = "test.rs";

    // TODO: figure this out and make tests less repetitive
    // type ParseTestCallback<'a> = dyn Fn(Result<Vec<ParsedSpec<'a>>, Vec<ParseError<'a>>>) -> Result<(), Box<dyn Err>> + 'a;

    // fn parser_test_without_ignoring<'a>(
    //     file_name: &'static str,
    //     content: &'static str,
    //     cb: Box<ParseTestCallback<'a>>,
    // ) -> Result<(), Box<dyn Err>> {
    //     let dir = TempDir::new()?;
    //     let repo = Repository::init(dir.path())?;
    //     let oids_to_blob_files = create_test_file_blob(&dir, &repo, file_name, content)?;
    //     let ignoring = BTreeSet::new();

    //     Parser::new(&oids_to_blob_files).parse(&ignoring);
    //     Ok(())
    // }

    fn to_non_zero_range(range: std::ops::Range<usize>) -> std::ops::Range<NonZeroUsize> {
        NonZeroUsize::new(range.start).unwrap()..NonZeroUsize::new(range.end).unwrap()
    }

    #[test]
    fn test_good_no_blocks() -> Result<(), Box<dyn Err>> {
        let dir = TempDir::new()?;
        let repo = Repository::init(dir.path())?;

        let oids_to_blob_files = create_test_file_blob(
            &dir,
            &repo,
            FILE_EXT_RS,
            "
        // ...
        // Line below is tricky, but should not match
        // ChangeTogether
        // ...
",
        )?;
        let ignoring = BTreeSet::new();

        match Parser::new(&oids_to_blob_files).parse(&ignoring) {
            Ok(specs) => {
                assert_eq!(specs.len(), 0);
                Ok(())
            }
            Err(errs) => Err(anyhow!("unexpected errors: {:#?}", errs).into()),
        }
    }

    #[test]
    fn test_good_full_file_anonymous_block() -> Result<(), Box<dyn Err>> {
        let dir = TempDir::new()?;
        let repo = Repository::init(dir.path())?;

        let oids_to_blob_files = create_test_file_blob(
            &dir,
            &repo,
            FILE_EXT_RS,
            "
        // ![[ChangeTogether.Start]]
        // ...
        // ![[ChangeTogether.End]]
",
        )?;
        let ignoring = BTreeSet::new();

        match Parser::new(&oids_to_blob_files).parse(&ignoring) {
            Ok(specs) => {
                assert_eq!(specs.len(), 1);

                let spec = &specs[0];
                assert_eq!(spec.file.ends_with(FILE_EXT_RS), true);
                assert_eq!(spec.block, to_non_zero_range(2..5));
                assert_eq!(spec.content, to_non_zero_range(3..4));
                assert_eq!(spec.tag.is_none(), true);
                assert_eq!(spec.links.is_empty(), true);
                Ok(())
            }
            Err(errs) => Err(anyhow!("unexpected errors: {:#?}", errs).into()),
        }
    }

    #[test]
    fn test_good_full_file_named_block() -> Result<(), Box<dyn Err>> {
        let dir = TempDir::new()?;
        let repo = Repository::init(dir.path())?;

        let oids_to_blob_files = create_test_file_blob(
            &dir,
            &repo,
            FILE_EXT_RS,
            "
        // ![[ChangeTogether.Start(myTag)]]
        // ...
        // ![[ChangeTogether.End(myTag)]]
",
        )?;
        let ignoring = BTreeSet::new();

        match Parser::new(&oids_to_blob_files).parse(&ignoring) {
            Ok(specs) => {
                assert_eq!(specs.len(), 1);

                let spec = &specs[0];
                assert_eq!(spec.file.ends_with(FILE_EXT_RS), true);
                assert_eq!(spec.block, to_non_zero_range(2..5));
                assert_eq!(spec.content, to_non_zero_range(3..4));
                assert_eq!(spec.tag.as_ref().unwrap().text, "myTag");
                assert_eq!(spec.links.is_empty(), true);
                Ok(())
            }
            Err(errs) => Err(anyhow!("unexpected errors: {:#?}", errs).into()),
        }
    }

    #[test]
    fn test_good_block_with_file_links() -> Result<(), Box<dyn Err>> {
        let dir = TempDir::new()?;
        let repo = Repository::init(dir.path())?;

        let oids_to_blob_files = create_test_file_blob(
            &dir,
            &repo,
            FILE_EXT_RS,
            r##"
        // ![[ChangeTogether.Start]]
        // ![[ChangeTogether.With("/foo/bar.rs")]]
        // ![[ChangeTogether.With("/baz/qux.rs")]]
        fn main() {
            println!("Hello World!");
        }
        // ![[ChangeTogether.End]]
"##,
        )?;
        let ignoring = BTreeSet::new();

        match Parser::new(&oids_to_blob_files).parse(&ignoring) {
            Ok(specs) => {
                assert_eq!(specs.len(), 1);

                let spec = &specs[0];
                assert_eq!(spec.file.ends_with(FILE_EXT_RS), true);
                assert_eq!(spec.block, to_non_zero_range(2..9));
                assert_eq!(spec.content, to_non_zero_range(5..8));
                assert_eq!(spec.tag.is_none(), true);
                assert_eq!(spec.links.len(), 2);

                let links: Vec<&ParsedLink> = spec.links.iter().clone().collect();
                let link0 = links[0];
                assert_eq!(link0.file.path.as_os_str(), "/foo/bar.rs");
                assert_eq!(link0.tag.is_none(), true);
                let link1 = links[1];
                assert_eq!(link1.file.path.as_os_str(), "/baz/qux.rs");
                assert_eq!(link1.tag.is_none(), true);
                Ok(())
            }
            Err(errs) => Err(anyhow!("unexpected errors: {:#?}", errs).into()),
        }
    }

    #[test]
    fn test_good_block_with_file_links_tricky_chars() -> Result<(), Box<dyn Err>> {
        let dir = TempDir::new()?;
        let repo = Repository::init(dir.path())?;

        let oids_to_blob_files = create_test_file_blob(
            &dir,
            &repo,
            FILE_EXT_RS,
            r##"
        // ![[ChangeTogether.Start]]
        // ![[ChangeTogether.With("/foo/bar(![[]],).rs")]]
        // ![[ChangeTogether.With("/baz/qux(![[]],).rs")]]
        fn main() {
            println!("Hello World!");
        }
        // ![[ChangeTogether.End]]
"##,
        )?;
        let ignoring = BTreeSet::new();

        match Parser::new(&oids_to_blob_files).parse(&ignoring) {
            Ok(specs) => {
                assert_eq!(specs.len(), 1);

                let spec = &specs[0];
                assert_eq!(spec.file.ends_with(FILE_EXT_RS), true);
                assert_eq!(spec.block, to_non_zero_range(2..9));
                assert_eq!(spec.content, to_non_zero_range(5..8));
                assert_eq!(spec.tag.is_none(), true);
                assert_eq!(spec.links.len(), 2);

                let links: Vec<&ParsedLink> = spec.links.iter().clone().collect();
                let link0 = links[0];
                assert_eq!(link0.file.path.as_os_str(), "/foo/bar(![[]],).rs");
                assert_eq!(link0.tag.is_none(), true);
                let link1 = links[1];
                assert_eq!(link1.file.path.as_os_str(), "/baz/qux(![[]],).rs");
                assert_eq!(link1.tag.is_none(), true);
                Ok(())
            }
            Err(errs) => Err(anyhow!("unexpected errors: {:#?}", errs).into()),
        }
    }

    #[test]
    fn test_good_block_with_file_and_tag_links() -> Result<(), Box<dyn Err>> {
        let dir = TempDir::new()?;
        let repo = Repository::init(dir.path())?;

        let oids_to_blob_files = create_test_file_blob(
            &dir,
            &repo,
            FILE_EXT_RS,
            r##"
        // ![[ChangeTogether.Start]]
        // ![[ChangeTogether.With("/foo/bar.rs",tagA)]]
        // ![[ChangeTogether.With("/baz/qux.rs",tagB)]]
        fn main() {
            println!("Hello World!");
        }
        // ![[ChangeTogether.End]]
"##,
        )?;
        let ignoring = BTreeSet::new();

        match Parser::new(&oids_to_blob_files).parse(&ignoring) {
            Ok(specs) => {
                assert_eq!(specs.len(), 1);

                let spec = &specs[0];
                assert_eq!(spec.file.ends_with(FILE_EXT_RS), true);
                assert_eq!(spec.block, to_non_zero_range(2..9));
                assert_eq!(spec.content, to_non_zero_range(5..8));
                assert_eq!(spec.tag.is_none(), true);

                let links: Vec<&ParsedLink> = spec.links.iter().clone().collect();
                let link0 = links[0];
                assert_eq!(link0.file.path.as_os_str(), "/foo/bar.rs");
                assert_eq!(link0.tag.as_ref().unwrap().text, "tagA");

                let link1 = links[1];
                assert_eq!(link1.file.path.as_os_str(), "/baz/qux.rs");
                assert_eq!(link1.tag.as_ref().unwrap().text, "tagB");
                Ok(())
            }
            Err(errs) => Err(anyhow!("unexpected errors: {:#?}", errs).into()),
        }
    }

    #[test]
    fn test_good_md_file() -> Result<(), Box<dyn Err>> {
        let dir = TempDir::new()?;
        let repo = Repository::init(dir.path())?;

        let oids_to_blob_files = create_test_file_blob(
            &dir,
            &repo,
            FILE_EXT_MD,
            r##"
        <!-- ![[ChangeTogether.Start]] -->
        <!-- ![[ChangeTogether.With("/foo/bar.rs",tagA)]] -->
        <!-- ![[ChangeTogether.With("/baz/qux.rs",tagB)]] -->
        # This is my markdown file
        ...
        <!-- ![[ChangeTogether.End]] -->
"##,
        )?;
        let ignoring = BTreeSet::new();

        match Parser::new(&oids_to_blob_files).parse(&ignoring) {
            Ok(specs) => {
                assert_eq!(specs.len(), 1);

                let spec = &specs[0];
                assert_eq!(spec.file.ends_with(FILE_EXT_MD), true);
                assert_eq!(spec.block, to_non_zero_range(2..8));
                assert_eq!(spec.content, to_non_zero_range(5..7));
                assert_eq!(spec.tag.is_none(), true);

                let links: Vec<&ParsedLink> = spec.links.iter().clone().collect();
                let link0 = links[0];
                assert_eq!(link0.file.path.as_os_str(), "/foo/bar.rs");
                assert_eq!(link0.tag.as_ref().unwrap().text, "tagA");

                let link1 = links[1];
                assert_eq!(link1.file.path.as_os_str(), "/baz/qux.rs");
                assert_eq!(link1.tag.as_ref().unwrap().text, "tagB");
                Ok(())
            }
            Err(errs) => Err(anyhow!("unexpected errors: {:#?}", errs).into()),
        }
    }

    #[test]
    fn test_good_py_file() -> Result<(), Box<dyn Err>> {
        let dir = TempDir::new()?;
        let repo = Repository::init(dir.path())?;

        let oids_to_blob_files = create_test_file_blob(
            &dir,
            &repo,
            FILE_EXT_PY,
            r##"
        # ![[ChangeTogether.Start]]
        # ![[ChangeTogether.With("/foo/bar.rs",tagA)]]
        # ![[ChangeTogether.With("/baz/qux.rs",tagB)]]
        print("We are in python now")
        # ![[ChangeTogether.End]]
"##,
        )?;
        let ignoring = BTreeSet::new();

        match Parser::new(&oids_to_blob_files).parse(&ignoring) {
            Ok(specs) => {
                assert_eq!(specs.len(), 1);

                let spec = &specs[0];
                assert_eq!(spec.file.ends_with(FILE_EXT_PY), true);
                assert_eq!(spec.block, to_non_zero_range(2..7));
                assert_eq!(spec.content, to_non_zero_range(5..6));
                assert_eq!(spec.tag.is_none(), true);

                let links: Vec<&ParsedLink> = spec.links.iter().clone().collect();
                let link0 = links[0];
                assert_eq!(link0.file.path.as_os_str(), "/foo/bar.rs");
                assert_eq!(link0.tag.as_ref().unwrap().text, "tagA");

                let link1 = links[1];
                assert_eq!(link1.file.path.as_os_str(), "/baz/qux.rs");
                assert_eq!(link1.tag.as_ref().unwrap().text, "tagB");
                Ok(())
            }
            Err(errs) => Err(anyhow!("unexpected errors: {:#?}", errs).into()),
        }
    }
}
