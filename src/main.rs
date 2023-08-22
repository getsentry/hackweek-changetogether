use anyhow::{Context, Error};
use git2::{Blob, Repository};
use std::error::Error as Err;
use structopt::StructOpt;

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

    // Get the ignores from the commit message, parse all of the files touched by this diff, then
    // resolve all of the references in that parsed output. That should give us enough information
    // to perform the actual analysis (namely, that everything we expect to have changed has
    // actually changed).
    let ignores = message::get_ignores(&target_commit)?;

    // Get the diff between the two commits.
    let diff = repo.diff_tree_to_tree(
        Some(&target_commit.tree()?),
        Some(&base_commit.tree()?),
        None,
    )?;

    // Get the blobs of all files on the "new" side of the diff, aka all those touched by the
    // `target_commit`.
    let mut target_blobs = Vec::<Blob>::new();
    diff.foreach(
        &mut |delta, _float| {
            let file_path = delta.new_file().path().unwrap();
            let blob = target_commit
                .tree()
                .unwrap() // TODO: no unwrap!
                .get_path(file_path)
                .unwrap() // TODO: no unwrap!
                .to_object(&repo)
                .unwrap() // TODO: no unwrap!
                .into_blob()
                .unwrap(); // TODO: no unwrap!
            target_blobs.push(blob);
            true
        },
        None,
        None,
        None,
    )?;

    let parsed_specs = target_blobs.into_iter().try_fold(vec![], |mut acc, blob| {
        match parser::parse(blob, &ignores) {
            Ok(mut parsed_specs) => {
                acc.append(&mut parsed_specs);
                return Ok(acc);
            }
            Err(err) => return Err(err),
        }
    })?;
    let resolved = resolver::resolve(parsed_specs);
    todo!();
}

#[cfg(test)]
mod tests {
    use crate::app;
    use crate::testing::helpers::{TestCommit, create_test_repo};
    use std::{collections::HashMap, error::Error as Err};

    #[test]
    fn test_good_no_change_together() -> Result<(), Box<dyn Err>> {
        let data = vec![
            TestCommit {
                msg: "First commit".into(),
                files: HashMap::from([("a.md", "# Original\n")]),
            },
            TestCommit {
                msg: "Second commit".into(),
                files: HashMap::from([("a.md", "# Changed\n")]),
            },
        ];
        let repo = create_test_repo(&data)?;

        Ok(app(repo, "HEAD".into())?)
    }
}
