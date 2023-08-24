use std::{num::NonZeroUsize, path::{PathBuf, Path}};

use git2::{Oid, Blob};

pub(crate) struct FileOid {
    pub path: PathBuf,
    pub oid: Oid,
}

impl FileOid {
    pub fn new(path: PathBuf, oid: Oid) -> FileOid {
        FileOid { path, oid }
    }
}

pub(crate) struct BlobFile<'a> {
    pub path: PathBuf,
    pub blob: Blob<'a>,
}

impl<'a> BlobFile<'a> {
    pub fn new(path: PathBuf, blob: Blob<'a>) -> BlobFile {
        BlobFile { path, blob }
    }
}

/// Any line in a relevant git blob that is not pure whitespace.
#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub(crate) struct NonWhitespaceLine<'a> {
    pub num: NonZeroUsize,
    pub text: &'a str,
}

/// A location within a `BlobFile`, noting the line text, line number, position within that line
/// number, and the length of the span in question.
#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub(crate) struct Location<'a> {
    pub line: NonWhitespaceLine<'a>,
    pub pos: usize,
    pub len: usize,
}

impl<'a> Location<'a> {
    pub fn new(line: NonWhitespaceLine<'a>, pos: usize, len: usize) -> Location<'a> {
        Location { line, pos, len }
    }

    pub fn concat(&self, subsequent: Location<'a>) -> Location<'a> {
        Location::new(
            self.line,
            self.pos,
            subsequent.len + (subsequent.pos - self.pos),
        )
    }
}

/// A parsed filename.
#[derive(Eq, Hash, PartialEq)]
pub(crate) struct File<'a> {
    /// Refers to the position of the first letter of the file name, not the surrounding quotes.
    pub loc: Location<'a>,
    pub path: &'a Path,
}

impl<'a> File<'a> {
    pub fn new(path: &'a Path, loc: Location<'a>) -> File<'a> {
        File { loc, path }
    }
}

/// A tag that is used to refer to a single "block" region.
#[derive(Eq, Hash, PartialEq)]
pub(crate) struct Name<'a> {
    /// Refers to the position of the first letter of the text.
    pub loc: Location<'a>,
    pub text: &'a str,
}

impl<'a> Name<'a> {
    pub fn new(text: &'a str, loc: Location<'a>) -> Name<'a> {
        Name { loc, text }
    }
}
