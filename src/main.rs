use anyhow::{anyhow, Context, Error};
use git2::{Commit, Diff, DiffDelta, Repository};
use std::error::Error as Err;
use structopt::StructOpt;

use crate::common::BlobFile;
use crate::errors::PrintError;
use crate::message::get_ignores;
use crate::parser::Parser;
use crate::resolver::resolve;

mod common;
mod errors;
mod message;
mod parser;
mod resolver;
mod startup;
mod testing;

#[derive(StructOpt, Debug)]
struct Cli {
    #[structopt(long = "repo")]
    repo: String,

    #[structopt(long = "target_rev")]
    target_rev: String,
}

fn main() -> Result<(), Box<dyn Err>> {
    let args = Cli::from_args();
    let repo = startup::clone_repo(args.repo)?;
    Ok(app(repo, args.target_rev)?)
}

fn app(repo: Repository, target_rev: String) -> Result<(), Error> {
    let target_commit = repo
        .revparse_single(target_rev.as_str())?
        .peel_to_commit()
        .context(format!("Could not find target commit {}", target_rev))?;
    let base_commit = target_commit
        .parent(0)
        .context("Target commit {} has no parent")?;

    // Get the diff between the two commits.
    let diff = repo.diff_tree_to_tree(
        Some(&target_commit.tree()?),
        Some(&base_commit.tree()?),
        None,
    )?;
    let target_blob_files = deltas_to_blob_files(&diff, &target_commit, &repo)?;

    // Get the ignores from the commit message, parse all of the files touched by this diff, then
    // resolve all of the references in that parsed output. That should give us enough information
    // to perform the actual analysis (namely, that everything we expect to have changed has
    // actually changed).
    let ignores = get_ignores(&target_commit)?;
    let parsed_specs = Parser::new()
        .parse(target_blob_files, &ignores)
        .map_err(|errs| report_errors(errs))?;
    // TODO: better resolved error handling
    resolve(parsed_specs, diff.deltas()).map_err(|errs| anyhow!("TODO: collate resolution errors"))
}

/// Helper function for conveniently displaying all discovered errors from a single phase.
fn report_errors<'a>(errors: Vec<errors::ParseError<'a>>) -> Error {
    let len = errors.len();
    let msg = errors
        .into_iter()
        // TODO: the zero below is probably incorrect...
        .map(|e| format!("{}", e.print(0)))
        .collect::<Vec<_>>()
        .join("\n");

    anyhow::anyhow!(format!("Found {} errors:\n{}", len, msg))
}

/// Get the blobs of all files on the "new" side of the diff, aka all those touched by the
/// `target_commit`.
fn deltas_to_blob_files<'a>(
    diff: &'a Diff<'a>,
    commit: &'a Commit<'a>,
    repo: &'a Repository,
) -> Result<Vec<BlobFile<'a>>, Error> {
    let mut target_blob_files = Vec::<BlobFile>::new();
    let mut maybe_err = None;
    diff.foreach(
        &mut |delta, _float| match delta_to_blob_file(delta, commit, repo) {
            Ok(blob_file) => {
                target_blob_files.push(blob_file);
                true
            }
            Err(err) => {
                maybe_err = Some(err);
                false
            }
        },
        None,
        None,
        None,
    )?;
    match maybe_err {
        Some(err) => Err(err),
        None => Ok(target_blob_files),
    }
}

/// Convert a single delta to the blob of its "new" side.
fn delta_to_blob_file<'a>(
    delta: DiffDelta<'a>,
    commit: &'a Commit,
    repo: &'a Repository,
) -> Result<BlobFile<'a>, Error> {
    let file_path = delta.new_file().path().unwrap();
    Ok(BlobFile::new(
        file_path,
        commit
            .tree()?
            .get_path(file_path)?
            .to_object(repo)?
            .into_blob()
            .map_err(|_| anyhow!("Could not access file deltas"))?,
    ))
}

// #[cfg(test)]
// mod tests {
//     use crate::app;
//     use crate::testing::helpers::{create_test_repo, TestCommit};
//     use std::{collections::HashMap, error::Error as Err};

//     #[test]
//     fn test_good_no_change_together() -> Result<(), Box<dyn Err>> {
//         let data = vec![
//             TestCommit {
//                 msg: "First commit".into(),
//                 files: HashMap::from([("a.md", "# Original\n")]),
//             },
//             TestCommit {
//                 msg: "Second commit".into(),
//                 files: HashMap::from([("a.md", "# Changed\n")]),
//             },
//         ];
//         let repo = create_test_repo(&data)?;

//         Ok(app(repo, "HEAD".into())?)
//     }
// }
