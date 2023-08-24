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

// fn is_spec_changed(spec_path: PathBuf, content: Range<NonZeroUsize>, diff: &Diff) -> bool {
//     for delta in diff.deltas() {
//         let old_file = delta.old_file();
//         let new_file = delta.new_file();
//         if new_file.path() == Some(spec_path.as_path()) {
//             for hunk in delta.hunks() {
//                 for line in hunk.lines_in_hunk() {
//                     if let Ok((line_number, _)) = line {
//                         if content.contains(&line_number) {
//                             return true;
//                         }
//                     }
//                 }
//             }
//         }
//     }
//     false
// }

fn resolve_spec(link: &ParsedLink, diff: &Diff) -> Result<(), Error> {
    let foo = link.file.path.to_string_lossy();
    let file_path = foo.trim_start_matches('/');
    for delta in diff.deltas() {
        if let Some(delta_path) = delta.new_file().path() {
            println!("delta path: {:#?}, file path: {}", delta_path, file_path);
            if delta_path.to_string_lossy() == file_path {
                return Ok(());
            }
        }
    }
    Err(anyhow!("Linked file not changed: {}", file_path))
}

pub(crate) fn resolve<'a>(
    specs: Vec<ParsedSpec>,
    diff: &Diff,
) -> Result<(), Error> {
    let mut errs = vec![];
    println!("specs: {:#?}", specs);

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
        let combine_errors = errs.iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<String>>()
                            .join("\n");
        Err(anyhow!(format!("Found {} errors:\n{}", errs.len(), combine_errors)))
    }
}
