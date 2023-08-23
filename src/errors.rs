use thiserror::Error;

use crate::common::Location;

pub trait PrintError {
    fn print(&self, leading_whitespace: usize) -> String;
}

/// An exhaustive enumeration of all of the parsing errors we can encounter.
#[derive(Debug, Clone, Error)]
pub(crate) enum ParseErrorKind {
    #[error("ChangeTogether lines must start with exactly one `!`, found {counted:?}")]
    SpellingLeadingBangs { counted: usize },

    #[error("ChangeTogether lines must not contain spaces between `!` and `[`, found {counted:?}")]
    SpellingSpacesAfterBang { counted: usize },

    #[error("ChangeTogether lines must start with exactly two `[`, found {counted:?}")]
    SpellingOpenBrackets { counted: usize },

    #[error("ChangeTogether lines must not contain spaces after `[`, found {counted:?}")]
    SpellingSpacesAfterOpenBrackets { counted: usize },

    #[error("ChangeTogether lines must end with exactly two `]`, found {counted:?}")]
    SpellingCloseBrackets { counted: usize },

    #[error("ChangeTogether lines must end with exactly two `]`, found extra text {extra:?}")]
    SpellingExtra { extra: String },

    #[error(
        "ChangeTogether lines must open with `![[ChangeTogether.`, you wrote `![[{keyword:?}`"
    )]
    SpellingKeyword { keyword: String },

    #[error("Invalid token encountered")]
    InvalidToken,

    #[error("Parentheses were not closed")]
    UnclosedParen,

    #[error("Unexpected token")]
    UnexpectedToken,

    #[error("Empty `ChangeTogether` statements are not allowed")]
    EmptyDecl,

    #[error("Unknown `ChangeTogether` declaration kind `{word:?}`")]
    UnknownMethod { word: String },

    #[error("Unknown `ChangeTogether` declaration kind `{word:?}`, did you mean `{suggest:?}`")]
    UnknownMethodSuggest { word: String, suggest: String },

    #[error("`ChangeTogether.Start/End` names must not be quoted")]
    UnnecessaryScopeString,

    #[error("`ChangeTogether.Start/End` declarations with no names must omit empty parentheses")]
    UnnecessaryParens,

    #[error("`ChangeTogether.With` declarations must include at least a file name string.")]
    EmptyWithDecl,

    #[error("`ChangeTogether.With` declarations must not have a trailing comma.")]
    UnnecessaryComa,
}

/// A single parsing error.
pub(crate) struct ParseError<'a> {
    pub kind: ParseErrorKind,
    pub loc: Location<'a>,
}

impl<'a> ParseError<'a> {
    pub fn new(kind: ParseErrorKind, loc: Location<'a>) -> ParseError<'a> {
        ParseError { kind, loc }
    }
}

impl<'a> PrintError for ParseError<'a> {
    fn print(&self, leading_whitespace: usize) -> String {
        // TODO: can probably have fewer allocations here...
        let offset =
            String::from_utf8(vec![b' '; leading_whitespace]).unwrap_or_else(|_| "".into());
        let whitespace = String::from_utf8(vec![b' '; leading_whitespace + self.loc.pos])
            .unwrap_or_else(|_| "".into());
        let fence = String::from_utf8(vec![b'^'; self.loc.len]).unwrap_or_else(|_| "".into());
        return format!(
            "{}{}\n{}{}\n",
            offset, self.loc.line.text, whitespace, fence
        );
    }
}
