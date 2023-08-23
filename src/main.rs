use anyhow::{anyhow, Context, Error};
use git2::{Blob, Commit, Diff, DiffDelta, Repository};
use std::error::Error as Err;
use structopt::StructOpt;

use crate::message::get_ignores;
// use crate::parser::parse;
// use crate::resolver::resolve;

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
    let target_blobs = deltas_to_blobs(&diff, &target_commit, &repo)?;

    // Get the ignores from the commit message, parse all of the files touched by this diff, then
    // resolve all of the references in that parsed output. That should give us enough information
    // to perform the actual analysis (namely, that everything we expect to have changed has
    // actually changed).
    let ignores = get_ignores(&target_commit)?;
    Ok(())
    // let parsed_specs = parse(target_blobs, &ignores).map_err(|errors| report_errors(errors))?;
    // resolve(parsed_specs, diff.deltas()).map_err(|errors| report_errors(errors))
}

/// Helper function for conveniently displaying all discovered errors from a single phase.
fn report_errors(errors: Vec<Error>) -> Error {
    let len = errors.len();
    let msg = errors
        .into_iter()
        .map(|e| format!("{}", e))
        .collect::<Vec<_>>()
        .join("\n");

    anyhow::anyhow!(format!("Found {} errors:\n{}", len, msg))
}

/// Get the blobs of all files on the "new" side of the diff, aka all those touched by the
/// `target_commit`.
fn deltas_to_blobs<'a>(
    diff: &'a Diff,
    commit: &'a Commit,
    repo: &'a Repository,
) -> Result<Vec<Blob<'a>>, Error> {
    let mut target_blobs = Vec::<Blob>::new();
    let mut maybe_err = None;
    diff.foreach(
        &mut |delta, _float| {
            match delta_to_blob(delta, commit, repo) {
                Ok(blob) => {
                    target_blobs.push(blob);
                    true
                }
                Err(err) => {
                    maybe_err = Some(err);
                    false
                }
            }
        },
        None,
        None,
        None,
    )?;
    match maybe_err {
        Some(err) => Err(err),
        None => Ok(target_blobs),
    }
}

/// Convert a single delta to the blob of its "new" side.
fn delta_to_blob<'a>(
    delta: DiffDelta,
    commit: &'a Commit,
    repo: &'a Repository,
) -> Result<Blob<'a>, Error> {
    let file_path = delta.new_file().path().unwrap();
    Ok(commit
        .tree()?
        .get_path(file_path)?
        .to_object(repo)?
        .into_blob()
        .map_err(|_| anyhow!("Could not access file deltas"))?) // TODO: better error message
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
