extern crate regex;

use std::{path::PathBuf, collections::BTreeSet};

use anyhow::Error;
use git2::Commit;
use regex::Regex;

/// Describes a commit-message level `CHANGE_TOGETHER_IGNORE` statement.
#[derive(Debug, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub(crate) enum Ignore<'a> {
    /// A commit-message ignore like `CHANGE_TOGETHER_IGNORE=/path/to/my/file`
    File(PathBuf),

    /// A commit-message ignore like `CHANGE_TOGETHER_IGNORE=#myCoolTag`
    Tag(&'a str),

    /// A commit-message ignore like `CHANGE_TOGETHER_IGNORE=/path/to/my/file#myCoolTag`
    FileTag(PathBuf, &'a str),

    /// A commit-message ignore like `CHANGE_TOGETHER_IGNORE=*`
    All,
}

// Returns a list of ignored files/tags to be disregarded during the resolving process.
pub(crate) fn get_ignores<'a>(commit: &'a Commit) -> Result<BTreeSet<Ignore<'a>>, Error> {
    let commit_message = commit.message().unwrap_or("");
    let pattern = r"CHANGE_TOGETHER_IGNORE=([^\s]+)";
    let re = Regex::new(pattern).unwrap();

    let mut ignore_list: BTreeSet<Ignore> = BTreeSet::new();

    // Iterate through all matches in the commit message and collect them
    // TODO: add error handling
    for captures in re.captures_iter(commit_message) {
        if let Some(matched_text) = captures.get(1) {
            let path_and_tag: &'a str  = matched_text.as_str();
            if path_and_tag == "*" {
                let mut hash_set_with_all: BTreeSet<Ignore> = BTreeSet::new();
                hash_set_with_all.insert(Ignore::All);
                return Ok(hash_set_with_all);
            }
            else if path_and_tag.starts_with("/") && path_and_tag.contains("#") {
                let parts: Vec<&str> = path_and_tag.split("#").collect();
                let path = parts[0];
                let tag = parts[1];
                ignore_list.insert(Ignore::FileTag(PathBuf::from(path), tag));
            }
            else if path_and_tag.starts_with("#") {
                let parts: Vec<&str> = path_and_tag.split("#").collect();
                let tag = parts[1];
                ignore_list.insert(Ignore::Tag(tag));
            }
            else if path_and_tag.starts_with("/") {
                ignore_list.insert(Ignore::File(PathBuf::from(path_and_tag)));
            }
            else {
                // TODO: Better error handling
                return Err(Error::msg("Error parsing commit message"));
            }
        }
    }
    Ok(ignore_list)
}
