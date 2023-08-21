use structopt::StructOpt;

#[derive(StructOpt, Debug)]
struct Cli {
    #[structopt(long = "repo")]
    repo: String,

    #[structopt(long = "commitSHA")]
    commit_sha: String,

    #[structopt(long = "baseSHA")]
    base_sha: String,
}


fn main() {
    // Get the command-line arguments
    let args = Cli::from_args();
}
