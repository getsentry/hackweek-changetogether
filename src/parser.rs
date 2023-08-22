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
pub(crate) struct ParsedSpec {
    pub file: PathBuf,
    pub tag: Option<String>,
    pub start: NonZeroUsize,
    pub content: Range<NonZeroUsize>,
    pub end: NonZeroUsize,
    pub links: HashSet<ParsedLink>,
}

/// Takes a file text and extract
pub(crate) fn parse<'a, 'b>(
    blobs: Vec<Blob<'a>>,
    ignoring: &'b HashSet<Ignore<'a>>,
) -> Result<Vec<ParsedSpec>, Vec<Error>> {
    blobs.into_iter().try_fold(vec![], |mut acc, blob| {
        match parse_blob(blob, ignoring) {
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
