use clap::{Arg, Command};
use git2::{Blob, Repository};
use std::error::Error;
use structopt::StructOpt;

mod resolver;
mod message;
mod parser;
mod startup;

#[derive(StructOpt, Debug)]
struct Cli {
    #[structopt(long = "repo")]
    repo: String,

    #[structopt(long = "target_sha")]
    target_sha: String,

    #[structopt(long = "base_sha")]
    base_sha: String,
}


fn main() -> Result<(), Box<dyn Error>> {
    // Handle the CLI input.
    let args = Cli::from_args();
    startup::clone_repo(args.repo);

    // TODO: get the git diff, so don't use "new"s here, but actually use the arguments.
    let repo = future_fn_to_get_repo(args.repo);
    let target_commit = future_fn_to_get_target_commit(args.target_sha);
    let base_commit = future_fn_to_get_base_commit_or_parent_of_target_commit(
        args.base_sha.or_else(target_commit.parent()),
    )?;

    // Get the ignores from the commit message, parse all of the files touched by this diff, then
    // resolve all of the references in that parsed output. That should give us enough information
    // to perform the actual analysis (namely, that everything we expect to have changed has
    // actually changed).
    ignores = message::get_ignores(target_commit);
    specs = vec![];
    for blob in target_commit.blobs() {
        parsed = parser::parse(blob, ignores)?;
        specs.concat(parsed);
    }
    resolved = resolver::compile(specs);

    future_fn_to_perform_analysis(repo.get_diff(target_commit, base_commit), resolved)?;
    Ok(())
}
