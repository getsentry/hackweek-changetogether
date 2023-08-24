#[cfg(test)]
pub(crate) mod helpers {
    use anyhow::{Context, Error};
    use git2::{IndexEntry, IndexTime, Oid, Repository};
    use std::collections::HashMap;
    use tempfile::TempDir;

    use crate::common::BlobFile;

    pub(crate) struct TestCommit<'a> {
        pub msg: &'a str,
        pub files: HashMap<&'a str, &'a str>,
    }

    /// Test against a custom git "file, represented as a blob. Useful for the parser. This function
    /// will return a map with a single item in it.
    pub(crate) fn create_test_file_blob<'a>(
        dir: &TempDir,
        repo: &'a Repository,
        file_name: &'static str,
        content: &'static str,
    ) -> Result<HashMap<Oid, BlobFile<'a>>, Error> {
        let file = dir.path().join(file_name).to_path_buf();
        let oid = repo.blob(content.as_bytes())?;
        let blob = repo.find_blob(oid)?;
        let blob_file = BlobFile::new(file, blob);
    
        let mut oids_to_blob_files = HashMap::new();
        oids_to_blob_files.insert(oid, blob_file);
        Ok(oids_to_blob_files)
    }

    /// Creates a repository with N commits in it.
    pub(crate) fn create_test_repo(
        dir: &TempDir,
        commits: &Vec<TestCommit>,
    ) -> Result<Repository, Error> {
        let mut repo =
            Repository::init_bare(dir.path()).context("could not create in-memory repo")?;
        commits
            .into_iter()
            .try_for_each(|test_commit| create_test_commit(&mut repo, test_commit))?;
        Ok(repo)
    }

    /// An empty index entry.
    fn create_empty_entry() -> IndexEntry {
        IndexEntry {
            ctime: IndexTime::new(0, 0),
            mtime: IndexTime::new(0, 0),
            dev: 0,
            ino: 0,
            mode: 0o100644,
            uid: 0,
            gid: 0,
            file_size: 0,
            id: Oid::from_bytes(&[0; 20]).unwrap(),
            flags: 0,
            flags_extended: 0,
            path: Vec::new(),
        }
    }

    fn create_test_commit(repo: &mut Repository, data: &TestCommit) -> Result<(), Error> {
        let mut index = repo.index()?;
        index.clear()?;

        // Add files.
        for (file_path, file_blob) in &data.files {
            let mut entry = create_empty_entry();
            entry.path = (*file_path).into();
            index.add_frombuffer(&entry, file_blob.as_bytes())?;
        }

        // Commit the built up index.
        let oid = index.write_tree()?;
        let tree = repo.find_tree(oid)?;
        let sig = repo.signature()?;

        // Include a parent reference if one exists, then write the commit.
        match repo.head() {
            Ok(head) => {
                let parent = head.peel_to_commit()?;
                repo.commit(Some("HEAD"), &sig, &sig, data.msg, &tree, &[&parent])?;
            }
            Err(_) => {
                repo.commit(Some("HEAD"), &sig, &sig, data.msg, &tree, &[])?;
            }
        };
        Ok(())
    }
}
