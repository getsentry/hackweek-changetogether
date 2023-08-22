use git2::{Blob, Deltas};
use std::collections::HashSet;
use std::num::NonZeroUsize;
use std::ops::Range;
use std::path::PathBuf;

use anyhow::Error;

use crate::parser::ParsedSpec;

/// Some lines from a git blob.
struct BlobSegment<'a> {
    pub blob: Blob<'a>,
    pub range: Range<NonZeroUsize>,
}

/// Very similar to a `parser::ParsedSpec`, except the references are resolved.
struct ResolvedSpec<'a> {
    pub file: PathBuf,
    pub tag: Option<String>,
    pub start: NonZeroUsize,
    pub end: NonZeroUsize,
    pub links: HashSet<BlobSegment<'a>>,
}

pub(crate) fn resolve<'a>(
    specs: Vec<ParsedSpec>,
    deltas: Deltas,
) -> Result<(), Vec<Error>> {
    todo!()
}
