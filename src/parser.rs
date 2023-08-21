use git2::Blob;
use std::collections::HashSet;
use std::num::NonZeroUsize;
use std::path::PathBuf;
use anyhow::Error;

use crate::message::Ignore;

/// Node storing the information in a single ` [[ChangeTogether.With(...)]] declaration.
pub(crate) struct ParsedLink {
    file: PathBuf,
    tag: Option<String>,
}

/// A `ParsedSpec` defines a single `Start...End` block. All names and line numbers are relative to
/// the new state, not that of the commit being diffed against.
pub(crate) struct ParsedSpec {
    file: PathBuf,
    tag: Option<String>,
    start: NonZeroUsize,
    end: NonZeroUsize,
    links: HashSet<ParsedLink>,
}

/// Takes a file text and extract
pub(crate) fn parse<'a, 'b>(blob: Blob<'a>, ignoring: &'b HashSet<Ignore<'a>>) -> Result<Vec<ParsedSpec>, Error> {
    todo!()
}

#[cfg(test)]
mod tests {}
