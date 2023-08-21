use anyhow::{Context};
use git2::{Blob, Repository};
use std::error::Error;
use structopt::StructOpt;

mod message;
mod parser;
mod resolver;
mod startup;

#[derive(StructOpt, Debug)]
struct Cli {
    #[structopt(long = "repo")]
    repo: String,

    #[structopt(long = "target_rev")]
    target_rev: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    // Handle the CLI input.
    let args = Cli::from_args();
    let repo = startup::clone_repo(args.repo)?;

    let target_commit = repo
        .revparse_single(args.target_rev.as_str())?
        .peel_to_commit()
        .context(format!("Could not find target commit {}", args.target_rev))?;
    let base_commit = target_commit
        .parent(1)
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
    // `target_commit.
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

    // future_fn_to_perform_analysis(repo.get_diff(target_commit, base_commit), resolved)?;
    todo!();
}
