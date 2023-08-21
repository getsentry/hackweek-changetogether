use clap::{Arg, Command};
use git2::{Blob, Repository};
use std::error::Error;

mod resolver;
mod message;
mod parser;

fn main() -> Result<(), Box<dyn Error>> {
    // Handle the CLI input.
    let matches = Command::new("change-together")
        .version("0.0.1")
        // TODO: add author and about info.
        .arg(
            Arg::new("REPO_PATH")
                .help("Path to the git repository")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::new("TARGET_COMMIT_REF")
                .help("Reference to the target commit (e.g., branch or commit hash)")
                .required(true)
                .index(2),
        )
        .arg(
            Arg::new("BASE_COMMIT_REF")
                .help("Optional reference to the base commit (e.g., branch or commit hash)")
                .required(false)
                .index(3),
        )
        .get_matches();

    // Get the arguments.
    let repo_path = matches
        .try_get_one::<String>("REPO_PATH")
        .expect("Could not parse `REPO_PATH` argument")?;
    let target_commit_ref = matches
        .try_get_one::<String>("TARGET_COMMIT_REF")
        .expect("Could not parse `TARGET_COMMIT_REF` argument")?;
    let base_commit_ref = matches
        .try_get_one::<Option<String>>("BASE_COMMIT_REF")
        .expect("Could not parse `BASE_COMMIT_REF` argument")?;

    // TODO: get the git diff, so don't use "new"s here, but actually use the arguments.
    let repo = future_fn_to_get_repo(repo_path)?;
    let target_commit = future_fn_to_get_target_commit(target_commit_ref)?;
    let base_commit = future_fn_to_get_base_commit_or_parent_of_target_commit(
        base_commit_ref.or_else(target_commit.parent()),
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
