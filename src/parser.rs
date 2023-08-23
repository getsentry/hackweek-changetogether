use anyhow::{anyhow, Error};
use edit_distance::edit_distance;
use git2::Blob;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::{HashMap, HashSet, VecDeque};
use std::ffi::OsStr;
use std::num::NonZeroUsize;
use std::ops::Range;
use std::path::Path;
use std::str::from_utf8;
use substring::Substring;

use crate::common::{BlobFile, Location, NonWhitespaceLine};
use crate::errors::{ParseErrorKind, ParseError};
use crate::message::Ignore;

lazy_static! {
    static ref WHITESPACE_REGEX: Regex = Regex::new(r"^\s*$").unwrap();

    // This purposefully includes extra characters so that we can catch common misspellings.
    static ref UNWRAPPING_REGEX: Regex =
        Regex::new(r####"(?P<bang>!*)(?P<gap0>\s*)(?P<open>\[+)(?P<gap1>\s*)(?P<keyword>\w{12,}\.)(?P<decl>.*)(?P<close>\]+)(?P<extra>.*)*$"####).unwrap();

    // TODO: we probably want some external source (or maybe even a config you can pass in?) for the
    // comment mapping, but we can use this for now.
    static ref EXTENSION_TO_COMMENT_REGEX: HashMap<&'static OsStr, Regex> = HashMap::from([
        (OsStr::new(".rs"), Regex::new(r"^\s*\/\/\\s*(?P<text>.*)$").unwrap()),
        (OsStr::new(".py"), Regex::new(r"^\s#\s*(?P<text>.*)$").unwrap()),
        (OsStr::new(".md"), Regex::new(r"^\s<!--\s*(?P<text>.*)-->$").unwrap()),
    ]);
}

/// Node storing the information in a single `![[ChangeTogether.With(...)]]` declaration.
pub(crate) struct ParsedLink<'a> {
    pub file: &'a Path,
    pub tag: Option<&'a str>,
}

// A `ParsedSpec` defines a single `Start...End` block. All names and line numbers are relative to
/// the new state, not that of the commit being diffed against.
pub(crate) struct ParsedSpec<'a> {
    // pub file: &'a Path,
    pub tag: Option<&'a str>,
    pub block: Range<NonZeroUsize>,
    pub content: Range<NonZeroUsize>,
    pub links: HashSet<ParsedLink<'a>>,
}

/// Takes a file text and extract
pub(crate) fn parse<'a, 'b>(
    blob_files: Vec<BlobFile<'a>>,
    ignoring: &'b HashSet<Ignore<'a>>,
) -> Result<Vec<ParsedSpec<'a>>, Vec<ParseError<'a>>> {
    // Record the first line that results in an error. This will prevent us from proceeding to the
    // next step.
    let mut fatal_err: Result<(), Error> = Ok(());
    let mut errs = vec![];

    let parsed_specs = blob_files
        .into_iter()
        .fold(vec![], |mut acc, blob_file| {
            match parse_blob_file(blob_file, ignoring, &mut errs) {
                Ok(mut parsed_specs) => {
                    acc.append(&mut parsed_specs);
                }
                Err(err) => {
                    // Record the failed compilation, but keep going for other blobs to recover more
                    // errors.
                    fatal_err = Err(err);
                },
            };
            acc
        });

    // If even a single compilation failed in a non-recoverable way, we do not proceed. 
    if let Err(err) = fatal_err  {
        return Err(errs);
    }
    Ok(parsed_specs)
}

/// A declaration and its source line, useful together for error reporting.
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

enum DeclData<'a> {
    Start(Option<Name<'a>>),
    With(File<'a>, Option<Name<'a>>),
    End(Option<Name<'a>>),
}

struct File<'a> {
    /// Refers to the position of the first letter of the file name, not the surrounding quotes.
    pub loc: Location<'a>,
    pub path: &'a Path,
}

impl<'a> File<'a> {
    fn new(path: &'a Path, loc: Location<'a>) -> File<'a> {
        File { loc, path }
    }
}

struct Name<'a> {
    /// Refers to the position of the first letter of the text.
    pub loc: Location<'a>,
    pub text: &'a str,
}

impl<'a> Name<'a> {
    fn new(text: &'a str, loc: Location<'a>) -> Name<'a> {
        Name { loc, text }
    }
}


fn parse_blob_file<'a, 'b>(
    blob_file: BlobFile<'a>,
    ignoring: &'b HashSet<Ignore<'a>>,
    errs: &'a mut Vec<ParseError<'a>>,
) -> Result<Vec<ParsedSpec<'a>>, Error> {
    let utf8 = match from_utf8(blob_file.blob.content()) {
        Ok(utf8) => utf8,
        // Non-UTF8 files are okay to have in the diff, but we ignore them for analysis purposes.
        Err(err) => return Ok(vec![]),
    };
    let mut errs = vec![];

    // Ignore files with unknown extensions.
    // TODO: clean up this nesting.
    let ext_regex = match blob_file.path.extension() {
        Some(ext) => match EXTENSION_TO_COMMENT_REGEX.get(ext) {
            Some(regex) => regex,
            // Unknown extension.
            None => return Ok(vec![]),
        },
        // No extension.
        None => return Ok(vec![]),
    };

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
            match parse_line(line, ext_regex, &mut errs) {
                Ok(decl_text) => match decl_text {
                    None => {},
                    Some(decl) => {
                        acc.push(decl);
                    }
                },
                Err(e) => {
                    return Err(e)
                },
            };
            Ok(acc)
        });

    todo!()
}

enum TokenData<'a> {
    Identifier(&'a str),
    OpenParen,
    StringContents(&'a str),
    CloseParen,
    Comma,
}

struct Token<'a> {
    pub loc: Location<'a>,
    pub data: TokenData<'a>,
}

fn parse_decl<'a>(
    line: NonWhitespaceLine<'a>,
    data: ParsableLineSegment<'a>,
    errs: &'a mut Vec<ParseError<'a>>,
) -> Result<Decl<'a>, Error> {
    let mut tokens = lex_decl(data.text, line, data.pos, errs)?;
    let method = match tokens.pop_front() {
        None => {
            errs.push(ParseError::new(
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
                return parse_scope_decl(method, true, tokens, errs);
            } else if word == "With" {
                return parse_with_decl(method, tokens, errs);
            } else if word == "End" {
                return parse_scope_decl(method, false, tokens, errs);
            }

            // Check for common misspellings, record the error and continue.
            if edit_distance(word, "Start") < 2 {
                errs.push(ParseError::new(
                    ParseErrorKind::UnknownMethodSuggest {
                        word: word.into(),
                        suggest: "Start".into(),
                    },
                    method.loc,
                ));
                return parse_scope_decl(method, true, tokens, errs);
            } else if edit_distance(word, "With") < 2 {
                errs.push(ParseError::new(
                    ParseErrorKind::UnknownMethodSuggest {
                        word: word.into(),
                        suggest: "With".into(),
                    },
                    method.loc,
                ));
                return parse_with_decl(method, tokens, errs);
            } else if edit_distance(word, "End") < 2 {
                errs.push(ParseError::new(
                    ParseErrorKind::UnknownMethodSuggest {
                        word: word.into(),
                        suggest: "End".into(),
                    },
                    method.loc,
                ));
                return parse_scope_decl(method, false, tokens, errs);
            }

            // We have no idea what the user wrote.
            errs.push(ParseError::new(
                ParseErrorKind::UnknownMethod { word: word.into() },
                method.loc,
            ));
            Err(anyhow!("method unrecognized"))
        }
        _ => {
            errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, method.loc));
            Err(anyhow!("unexpected token"))
        }
    }
}

// TODO: maybe return iterator instead of vec?
fn lex_decl<'a>(
    text: &'a str,
    line: NonWhitespaceLine<'a>,
    offset: usize,
    errs: &'a mut Vec<ParseError<'a>>,
) -> Result<VecDeque<Token<'a>>, Error> {
    let mut tokens = VecDeque::new();
    let mut from = 0;
    let mut in_string = false;
    for (i, ch) in text.chars().enumerate() {
        let len = (i - from) + 1;

        // Is this the end of a string?
        if in_string {
            if ch == '"' {
                tokens.push_back(Token {
                    loc: Location::new(line, from, i - from),
                    data: TokenData::Identifier(text.substring(from, i - from)),
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
            if from < i {
                tokens.push_back(Token {
                    loc: Location::new(line, from, i - from),
                    data: TokenData::Identifier(text.substring(from, i)),
                });
            }
            tokens.push_back(Token {
                loc: Location::new(line, from, 1),
                data: token_data,
            });
            from = i + 1;
            continue;
        }

        // Ignore whitespace, but use it to demarcate the end of a token if necessary.
        if ch == ' ' || ch == '\t' {
            if from < i {
                tokens.push_back(Token {
                    loc: Location::new(line, from, i - from),
                    data: TokenData::Identifier(text.substring(from, i)),
                });
            }
            from = i + 1;
        }

        // Only (valid) option left: we are inside an identifier.
        if ch.is_alphanumeric() {
            continue;
        }

        errs.push(ParseError::new(
            ParseErrorKind::InvalidToken,
            Location::new(line, i, 1),
        ));
        return Err(anyhow!("lexing failed"));
    }

    // Close any trailing identifiers.
    if from < text.len() {
        tokens.push_back(Token {
            loc: Location::new(line, from, text.len() - from),
            data: TokenData::Identifier(text.substring(from, text.len() - from)),
        });
    }

    Ok(tokens)
}

/// Parse a `Start` or `End` decl. These both have identical grammar, so we can parse them using the
/// same function, with a simple boolean differentiate which of the two declarations we're building.
/// Because the scope name argument is optional, these declarations can take one of two forms
/// (examples below are for `Start`, but would be identical for `End`):
///  
/// 1. `Start`
/// 2. `Start(myRegion)`
///
/// Note that the front `With` token has already been processed by the time we enter this function,
/// so we just need to handle the parentheses and whatever is contained within.
fn parse_scope_decl<'a>(
    method_token: Token,
    // TODO: should probably add `Start`, `End`, and `With` token kinds instead of this bool.
    is_start: bool,
    mut tokens: VecDeque<Token>,
    errs: &'a mut Vec<ParseError<'a>>,
) -> Result<Decl<'a>, Error> {
    // Since we have already parsed the method name token, it is possible for there to be no tokens
    // left if we have a no-argument declaration. If so, we can just exit right here.
    let mut loc = match tokens.pop_front() {
        None => return Ok(Decl::new(DeclData::Start(None), method_token.loc)),
        Some(token) => match token.data {
            TokenData::OpenParen => method_token.loc.concat(token.loc),
            _ => {
                errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                return Err(anyhow!("unexpected token"));
            }
        },
    };

    // We are looking at the one-argument variant - parse the name.
    let name = match tokens.pop_front() {
        None => {
            errs.push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
            return Err(anyhow!("unclosed paren"));
        }
        Some(token) => match token.data {
            TokenData::Identifier(contents) => Name::new(contents, token.loc),
            TokenData::StringContents(contents) => {
                errs.push(ParseError::new(
                    ParseErrorKind::UnnecessaryScopeString,
                    token.loc,
                ));
                Name::new(contents, token.loc)
            }
            TokenData::CloseParen => {
                errs.push(ParseError::new(
                    ParseErrorKind::UnnecessaryParens,
                    token.loc,
                ));
                return Ok(Decl::new(DeclData::Start(None), loc.concat(token.loc)));
            }
            _ => {
                errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                return Err(anyhow!("unexpected token"));
            }
        },
    };

    // If we've parsed the name successfully must be a closing paren.
    loc.concat(name.loc);
    loc = match tokens.pop_front() {
        None => {
            errs.push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
            return Err(anyhow!("unclosed paren"));
        }
        Some(token) => match token.data {
            TokenData::CloseParen => loc.concat(token.loc),
            _ => {
                errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                return Err(anyhow!("unexpected token"));
            }
        },
    };

    // Make sure there are no extra tokens after the end.
    match tokens.pop_front() {
        Some(token) => {
            errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
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
/// argument may optionally be quoted (to allow most weird characters in paths), so we are parsing 4
/// kinds of declarations:
///
/// 1. `With(/my/fun/file.ext)`
/// 2. `With("/my/fun/file.ext")`
/// 3. `With(/my/fun/file.ext, someTag)`
/// 4. `With("/my/fun/file.ext", someTag)`
///
/// Note that the front `With` token has already been processed by the time we enter this function,
/// so we just need to handle the parentheses and whatever is contained within.
fn parse_with_decl<'a>(
    method_token: Token,
    mut tokens: VecDeque<Token>,
    errs: &'a mut Vec<ParseError<'a>>,
) -> Result<Decl<'a>, Error> {
    let mut has_second_arg = false;
    let mut loc = match tokens.pop_front() {
        None => {
            errs.push(ParseError::new(
                ParseErrorKind::EmptyWithDecl,
                method_token.loc,
            ));
            return Err(anyhow!("empty with decl"));
        }
        Some(token) => match token.data {
            TokenData::OpenParen => method_token.loc.concat(token.loc),
            _ => {
                errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                return Err(anyhow!("unexpected token"));
            }
        },
    };

    let file = match tokens.pop_front() {
        None => {
            errs.push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
            return Err(anyhow!("unclosed paren"));
        }
        Some(token) => match token.data {
            TokenData::CloseParen => {
                errs.push(ParseError::new(
                    ParseErrorKind::EmptyWithDecl,
                    loc.concat(token.loc),
                ));
                return Err(anyhow!("unexpected token"));
            }
            // File paths may be optionally quoted, so either an `Identifier` or a `StringContents`
            // is acceptable here.
            TokenData::Identifier(contents) | TokenData::StringContents(contents) => {
                File::new(Path::new(contents), token.loc)
            }
            _ => {
                errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                return Err(anyhow!("unexpected token"));
            }
        },
    };

    // Parse the token after the mandatory file path. If it's a comma, assume we are parsing a
    // two-argument variant. If its a closing paren, assume it's the one-argument variant instead.
    loc.concat(file.loc);
    loc = match tokens.pop_front() {
        None => {
            errs.push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
            return Err(anyhow!("unclosed paren"));
        }
        Some(token) => match token.data {
            TokenData::CloseParen => {
                has_second_arg = true;
                loc.concat(token.loc)
            }
            TokenData::Comma => loc.concat(token.loc),
            _ => {
                errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                return Err(anyhow!("unexpected token"));
            }
        },
    };

    // If the last token was not comma, assume this is the one-argument variant.
    if has_second_arg {
        // Make sure there are no extra tokens after the end.
        return match tokens.pop_front() {
            Some(token) => {
                errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                Err(anyhow!("unexpected token"))
            }
            None => Ok(Decl::new(DeclData::With(file, None), loc)),
        };
    }

    // Parse the second tag name argument.
    let name = match tokens.pop_front() {
        None => {
            errs.push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
            return Err(anyhow!("unclosed paren"));
        }
        Some(token) => match token.data {
            TokenData::Identifier(contents) => Name::new(contents, token.loc),
            TokenData::StringContents(contents) => {
                errs.push(ParseError::new(
                    ParseErrorKind::UnnecessaryScopeString,
                    token.loc,
                ));
                Name::new(contents, token.loc)
            }
            TokenData::CloseParen => {
                errs.push(ParseError::new(ParseErrorKind::UnnecessaryComa, token.loc));
                return Ok(Decl::new(DeclData::Start(None), loc.concat(token.loc)));
            }
            _ => {
                errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                return Err(anyhow!("unexpected token"));
            }
        },
    };

    // The next token must be the closing paren.
    loc.concat(name.loc);
    loc = match tokens.pop_front() {
        None => {
            errs.push(ParseError::new(ParseErrorKind::UnclosedParen, loc));
            return Err(anyhow!("unclosed paren"));
        }
        Some(token) => match token.data {
            TokenData::CloseParen => loc.concat(token.loc),
            _ => {
                errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
                return Err(anyhow!("unexpected token"));
            }
        },
    };

    // Make sure there are no extra tokens after the end.
    return match tokens.pop_front() {
        Some(token) => {
            errs.push(ParseError::new(ParseErrorKind::UnexpectedToken, token.loc));
            Err(anyhow!("unexpected token"))
        }
        None => Ok(Decl::new(DeclData::With(file, Some(name)), loc)),
    };
}

// TODO: our parsing strategy for this is pretty wasteful, since we have to read the line twice via
// a regex before we even start parsing the annotation itself. We may want to look at doing more
// traditional lexing with a single pass to speed things up in the future.
fn parse_line<'a, 'b>(
    line: NonWhitespaceLine<'a>,
    comment_regex: &'b Regex,
    errs: &'a mut Vec<ParseError<'a>>,
) -> Result<Option<Decl<'a>>, Error> {
    // See if we can get a matching comment line structure inside the brackets. If we can't, we
    // assume that this was not a correct comment line. Note that the `UNWRAPPING_REGEX` is pretty
    // forgiving of errors, so we can catch some obvious misspellings here.
    let parsable_segment = match unwrap_line_contents(line, comment_regex, errs)? {
        Some(unbracketed_text) => unbracketed_text,
        None => return Ok(None),
    };

    match parse_decl(line, parsable_segment, errs) {
        Ok(decl) => Ok(Some(decl)),
        Err(err) => Err(err),
    }
}

#[derive(Copy, Clone)]
struct ParsableLineSegment<'a> {
    pub text: &'a str,
    pub pos: usize,
}

/// Is this even a comment line? Does it have content? Does it have the correct brackets? Three
/// possible answers:
///
/// 1. This is not a comment line, or at least not one we think is targeted at `ChangeTogether`,
///    return `Ok(None)`.
/// 2. This is an invalidly spelled `ChangeTogether` line, return an `Err(...)`.
/// 3. This is a valid `ChangeTogether` line, in which case we return `Ok(Some(...))`.
fn unwrap_line_contents<'a, 'b>(
    line: NonWhitespaceLine<'a>,
    comment_regex: &'b Regex,
    errs: &'a mut Vec<ParseError<'a>>,
) -> Result<Option<ParsableLineSegment<'a>>, Error> {
    let err_count = errs.len();
    let comment = match comment_regex.captures(line.text) {
        None => return Ok(None),
        Some(matched) => matched,
    };
    let comment_text = match comment.name("text") {
        None => return Ok(None),
        Some(text) => text,
    };
    let comment_text_str = comment_text.as_str();

    // This is definitely a non-empty comment line, let's see if it is a `ChangeTogether` line, or a
    // close misspelling of one.
    let matches = match UNWRAPPING_REGEX.captures(comment_text_str) {
        Some(matches) => matches,
        None => return Ok(None),
    };

    // The unwraps below are safe, because we want to always panic if one of these named groups was
    // not found in our regex, as this is a programmer error.
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
        errs.push(ParseError::new(
            ParseErrorKind::SpellingKeyword {
                keyword: keyword.as_str().to_string(),
            },
            Location::new(line, keyword.start(), keyword.len()),
        ));
    }

    // TODO: this repetition below is painful, clean this up.
    if bangs.len() != 1 {
        errs.push(ParseError::new(
            ParseErrorKind::SpellingLeadingBangs {
                counted: bangs.len(),
            },
            Location::new(line, bangs.start(), bangs.len()),
        ));
    }
    if gap0.len() != 0 {
        errs.push(ParseError::new(
            ParseErrorKind::SpellingSpacesAfterBang {
                counted: gap0.len(),
            },
            Location::new(line, gap0.start(), gap0.len()),
        ));
    }
    if open.len() != 2 {
        errs.push(ParseError::new(
            ParseErrorKind::SpellingOpenBrackets {
                counted: open.len(),
            },
            Location::new(line, open.start(), open.len()),
        ));
    }
    if gap1.len() != 0 {
        errs.push(ParseError::new(
            ParseErrorKind::SpellingSpacesAfterOpenBrackets {
                counted: gap1.len(),
            },
            Location::new(line, gap1.start(), gap1.len()),
        ));
    }
    if close.len() != 2 {
        errs.push(ParseError::new(
            ParseErrorKind::SpellingCloseBrackets {
                counted: close.len(),
            },
            Location::new(line, close.start(), close.len()),
        ));
    }
    if extra.len() != 0 {
        let ex = extra.as_str();
        if !ex.chars().all(|ch| ch.is_whitespace()) {
            errs.push(ParseError::new(
                ParseErrorKind::SpellingExtra {
                    extra: ex.to_string(),
                },
                Location::new(line, close.start(), close.len()),
            ));
        }
    }

    Ok(Some(ParsableLineSegment {
        text: decl.as_str(),
        pos: decl.start(),
    }))
}

#[cfg(test)]
mod tests {}
