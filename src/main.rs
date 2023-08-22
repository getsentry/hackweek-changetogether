use anyhow::{Context, Error};
use git2::{Blob, Repository};
use std::error::Error as Err;
use structopt::StructOpt;

mod message;
mod parser;
mod resolver;
mod startup;

#[derive(StructOpt, Debug)]
struct Cli {
    #[structopt(long = "repo")]
    repo: String,

    #[structopt(long = "target_rev")]
    target_rev: String,
}

fn main() -> Result<(), Box<dyn Err>> {
    let args = Cli::from_args();
    let repo = startup::clone_repo(args.repo)?;
    Ok(app(repo, args.target_rev)?)
}

fn app(repo: Repository, target_rev: String) -> Result<(), Error> {
    let target_commit = repo
        .revparse_single(target_rev.as_str())?
        .peel_to_commit()
        .context(format!("Could not find target commit {}", target_rev))?;
    let base_commit = target_commit
        .parent(0)
        .context("Target commit {} has no parent")?;

    // Get the ignores from the commit message, parse all of the files touched by this diff, then
    // resolve all of the references in that parsed output. That should give us enough information
    // to perform the actual analysis (namely, that everything we expect to have changed has
    // actually changed).
    let ignores = message::get_ignores(&target_commit)?;

    // Get the diff between the two commits.
    let diff = repo.diff_tree_to_tree(
        Some(&target_commit.tree()?),
        Some(&base_commit.tree()?),
        None,
    )?;

    // Get the blobs of all files on the "new" side of the diff, aka all those touched by the
    // `target_commit`.
    let mut target_blobs = Vec::<Blob>::new();
    diff.foreach(
        &mut |delta, _float| {
            let file_path = delta.new_file().path().unwrap();
            let blob = target_commit
                .tree()
                .unwrap() // TODO: no unwrap!
                .get_path(file_path)
                .unwrap() // TODO: no unwrap!
                .to_object(&repo)
                .unwrap() // TODO: no unwrap!
                .into_blob()
                .unwrap(); // TODO: no unwrap!
            target_blobs.push(blob);
            true
        },
        None,
        None,
        None,
    )?;

    let parsed_specs = target_blobs.into_iter().try_fold(vec![], |mut acc, blob| {
        match parser::parse(blob, &ignores) {
            Ok(mut parsed_specs) => {
                acc.append(&mut parsed_specs);
                return Ok(acc);
            }
            Err(err) => return Err(err),
        }
    })?;
    let resolved = resolver::resolve(parsed_specs);
    todo!();
}

#[cfg(test)]
mod tests {
    use anyhow::{anyhow, Context, Error};
    use git2::{IndexEntry, Repository, IndexTime, Oid};
    use std::{collections::HashMap, error::Error as Err, default};

    use crate::app;

    struct TestCommit<'a> {
        msg: &'a str,
        files: HashMap<&'static str, &'static str>,
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

    /// Creates a repository with two commits in it.
    fn create_test_repo(commits: &Vec<TestCommit>) -> Result<Repository, Error> {
        let mut repo =
            Repository::init_bare(":TESTING:").context("could not create in-memory repo")?;
        commits.into_iter().try_for_each(|test_commit| create_test_commit(&mut repo, test_commit))?;
        Ok(repo)
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
            },
            Err(_) => {
                repo.commit(Some("HEAD"), &sig, &sig, data.msg, &tree, &[])?;
            },
        };
        Ok(())
    }

    // fn setup_repo() -> Result<Repository, git2::Error> {
    //     // Initialize an in-memory repository
    //     let repo = Repository::init_bare(":memory:")?;

    //     // Create an initial commit
    //     {
    //         let tree_oid = {
    //             let mut index = repo.index()?;
    //             let oid = index.write_tree()?;
    //             oid
    //         };

    //         let tree = repo.find_tree(tree_oid)?;
    //         let sig = repo.signature()?;
    //         repo.commit(Some("HEAD"), &sig, &sig, "Initial commit", &tree, &[])?;
    //     }

    //     // Second commit with "bar" in it
    //     {
    //         let blob_oid = repo.blob(b"Hello, world! bar")?;

    //         let tree_oid = {
    //             let mut index = repo.index()?;
    //             index.add_frombuffer(&mut git2::IndexEntry {
    //                 path: "file.txt".into(),
    //                 oid: blob_oid,
    //                 ..Default::default()
    //             })?;
    //             let oid = index.write_tree()?;
    //             oid
    //         };

    //         let tree = repo.find_tree(tree_oid)?;
    //         let sig = repo.signature()?;
    //         repo.commit(
    //             Some("HEAD"),
    //             &sig,
    //             &sig,
    //             "Second commit",
    //             &tree,
    //             &[&repo.head()?.peel_to_commit()?],
    //         )?;
    //     }

    //     Ok(repo)
    // }

    #[test]
    fn test_good_no_change_together() -> Result<(), Box<dyn Err>> {
        let data = vec![
            TestCommit {
                msg: "First commit".into(),
                files: HashMap::from([("a.md", "# Original\n")]),
            },
            TestCommit {
                msg: "Second commit".into(),
                files: HashMap::from([("a.md", "# Changed\n")]),
            },
        ];
        let repo = create_test_repo(&data)?;

        Ok(app(repo, "HEAD".into())?)
    }
}
