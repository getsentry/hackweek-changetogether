use anyhow::Error;
use git2::Blob;
use std::collections::HashSet;
use std::num::NonZeroUsize;
use std::ops::Range;
use std::path::PathBuf;

use crate::message::Ignore;

/// Node storing the information in a single ` [[ChangeTogether.With(...)]] declaration.
pub(crate) struct ParsedLink {
    pub file: PathBuf,
    pub tag: Option<String>,
}

/// A `ParsedSpec` defines a single `Start...End` block. All names and line numbers are relative to
/// the new state, not that of the commit being diffed against.
pub(crate) struct ParsedSpec<'a> {
    pub file: &'a Path,
    pub tag: Option<&'a str>,
    pub block: Range<NonZeroUsize>,
    pub content: Range<NonZeroUsize>,
    pub links: HashSet<ParsedWith<'a>>,
}

/// A single parsing error.
pub(crate) struct ParseError<'a> {
    pub kind: ParseErrorKind,
    pub line: NonWhitespaceLine<'a>,
    pub pos: usize,
    pub len: usize,
}

impl<'a> PrintError for ParseError<'a> {
    fn print(&self, leading_whitespace: usize) -> String {
        // TODO: can probably have fewer allocations here...
        let offset = String::from_utf8(vec![b' '; leading_whitespace])
            .unwrap_or_else(|_| "".into());
        let whitespace = String::from_utf8(vec![b' '; leading_whitespace + self.pos])
            .unwrap_or_else(|_| "".into());
        let fence = String::from_utf8(vec![b'^'; self.len])
            .unwrap_or_else(|_| "".into());
        return format!("{}{}\n{}{}\n", offset, self.line.text, whitespace, fence);
    }
}

/// An exhaustive enumeration of all of the parsing errors we can encounter.
#[derive(Debug, Clone, ThisError)]
pub(crate) enum ParseErrorKind {
    #[error("Valid `ChangeTogether` lines must start with exactly one `!`, found {counted:?}")]
    SpellingLeadingBangs { counted: usize },
}

/// Takes a file text and extract
pub(crate) fn parse<'a, 'b>(
    blob_files: Vec<BlobFile<'a>>,
    ignoring: &'b HashSet<Ignore<'a>>,
) -> Result<Vec<ParsedSpec<'a>>, Vec<Error>> {
    blob_files
        .into_iter()
        .try_fold(vec![], |mut acc, blob_file| {
            match parse_blob_file(blob_file, ignoring) {
                Ok(mut parsed_specs) => {
                    acc.append(&mut parsed_specs);
                    return Ok(acc);
                }
                Err(err) => return Err(err),
            }
        })
}

fn parse_blob<'a, 'b>(
    blobs: Blob<'a>,
    ignoring: &'b HashSet<Ignore<'a>>,
) -> Result<Vec<ParsedSpec>, Vec<Error>> {
    todo!()
}

#[cfg(test)]
mod tests {}
