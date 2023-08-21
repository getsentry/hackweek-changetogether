use git2::Blob;
use std::collections::HashSet;
use std::path::PathBuf;
use std::num::NonZeroUsize;

use anyhow::Error;

use crate::parser::ParsedSpec;

/// Some lines from a git blob.
pub(crate) struct BlobSegment<'a> {
    blob: Blob<'a>,
    start: NonZeroUsize,
    end: NonZeroUsize,
}

/// Very similar to a `parser::ParsedSpec`, except the references are resolved.
pub(crate) struct ResolvedSpec<'a> {
    file: PathBuf,
    tag: Option<String>,
    start: NonZeroUsize,
    end: NonZeroUsize,
    links: HashSet<BlobSegment<'a>>,
}

pub(crate) fn resolve<'a>(specs: Vec<ParsedSpec>) -> Result<Vec<ResolvedSpec<'a>>, Error> {
    todo!()
}
