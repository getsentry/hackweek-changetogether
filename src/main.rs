use structopt::StructOpt;

mod startup;

#[derive(StructOpt, Debug)]
struct Cli {
    #[structopt(long = "repo")]
    repo: String,

    #[structopt(long = "commit_sha")]
    commit_sha: String,

    #[structopt(long = "base_sha")]
    base_sha: String,
}


fn main() {
    // Get the command-line arguments
    let args = Cli::from_args();
    startup::clone_repo(args.repo);
}
