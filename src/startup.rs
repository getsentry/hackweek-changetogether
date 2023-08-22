extern crate git2;

use anyhow::{Error, Context};
use git2::Repository;

pub(crate) fn clone_repo(repo: String) -> Result<Repository, Error> {
    let url = format!("https://github.com/{}", repo);
    Repository::clone(&url, format!("/tmp/{}", repo)).context("failed to clone")
}
