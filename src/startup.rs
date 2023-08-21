extern crate git2;

use git2::{Repository};

pub fn clone_repo(repo: String) {
    let url = format!("https://github.com/{}", repo);
    let repo = match Repository::clone(&url, format!("/tmp/{}", repo)) {
        Ok(repo) => repo,
        Err(e) => panic!("failed to clone: {}", e),
    };
}
