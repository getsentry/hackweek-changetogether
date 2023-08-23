use std::{num::NonZeroUsize, path::Path};

use git2::Blob;

pub(crate) struct BlobFile<'a> {
    pub path: &'a Path,
    pub blob: Blob<'a>,
}

impl<'a> BlobFile<'a> {
    pub fn new(path: &'a Path, blob: Blob<'a>) -> BlobFile<'a> {
        BlobFile { path, blob }
    }
}

/// Any line in a relevant git blob that is not pure whitespace.
#[derive(Copy, Clone)]
pub(crate) struct NonWhitespaceLine<'a> {
    pub num: NonZeroUsize,
    pub text: &'a str,
}

/// A location within a `BlobFile`, noting the line text, line number, position within that line
/// number, and the length of the span in question.
#[derive(Copy, Clone)]
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
