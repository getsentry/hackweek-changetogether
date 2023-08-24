use git2::{Blob, Diff};
use std::collections::HashSet;
use std::num::NonZeroUsize;
use std::ops::Range;
use std::path::PathBuf;

use anyhow::{anyhow, Error};

use crate::parser::{ParsedSpec, ParsedLink};

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

fn resolve_spec(link: &ParsedLink, diff: &Diff) -> Result<(), Error> {
    let file_path = link.file;
    for delta in diff.deltas() {
        if let Some(delta_path) = delta.new_file().path() {
            if delta_path == file_path {
                return Ok(());
            }
        }
    }
    Err(anyhow!("Linked file not changed"))
}

pub(crate) fn resolve<'a>(
    specs: Vec<ParsedSpec>,
    diff: &Diff,
) -> Result<(), Vec<Error>> {
    let mut errs = vec![];

    for spec in &specs {
        for link in spec.links.iter() {
            match resolve_spec(link, diff) {
                Ok(()) => {},
                Err(err) => {
                    errs.push(err);
                }
            };
        }
    }
    if errs.is_empty() {
        Ok(())
    } else {
        Err(errs)
    }
}