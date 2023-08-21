use std::{path::PathBuf, collections::HashSet};

use anyhow::Error;
use git2::Commit;

/// Describes a commit-message level `CHANGE_TOGETHER_IGNORE` statement.
pub(crate) enum Ignore<'a> {
    /// A commit-message ignore like `CHANGE_TOGETHER_IGNORE=/path/to/my/file`
    File(PathBuf),

    /// A commit-message ignore like `CHANGE_TOGETHER_IGNORE=#myCoolTag`
    Tag(&'a str),

    /// A commit-message ignore like `CHANGE_TOGETHER_IGNORE=/path/to/my/file#myCoolTag`
    FileTag(PathBuf, &'a str),

    /// A commit-message ignore like `CHANGE_TOGETHER_IGNORE=*`
    All(),
}

pub(crate) fn get_ignores<'a>(commit: &'a Commit) -> Result<HashSet<Ignore<'a>>, Error> {
    // TODO: fill in
    todo!()
}
