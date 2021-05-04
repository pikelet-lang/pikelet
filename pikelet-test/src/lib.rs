use libtest_mimic::{Outcome, Test};
use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use walkdir::WalkDir;

/// Recursively walk over test files under a file path.
pub fn walk_files(root: impl AsRef<Path>) -> impl Iterator<Item = PathBuf> {
    WalkDir::new(root)
        .into_iter()
        .filter_map(|dir_entry| dir_entry.ok())
        .filter(|dir_entry| dir_entry.file_type().is_file())
        .map(|dir_entry| dir_entry.into_path())
}

fn is_pikelet_path(path: &Path) -> bool {
    matches!(path.extension(), Some(ext) if ext == "pi")
}

pub fn extract_test(path: PathBuf) -> Option<Test<PathBuf>> {
    is_pikelet_path(&path).then(|| Test {
        name: path.display().to_string(),
        kind: String::new(),
        is_ignored: false,
        is_bench: false,
        data: path,
    })
}

pub fn run_test(
    pikelet_path: &'static str,
) -> impl Fn(&Test<PathBuf>) -> Outcome + 'static + Send + Sync {
    move |test| run_test_impl(pikelet_path, test)
}

fn run_test_impl(pikelet_path: &str, test: &Test<PathBuf>) -> Outcome {
    let path = test.data.display().to_string();
    let output = Command::new(pikelet_path)
        .arg("check")
        .arg("--validate-core")
        .arg(&path)
        .output();

    let output = match output {
        Ok(output) => output,
        Err(error) => {
            return Outcome::Failed {
                msg: Some(format!("Error running {}: {}", pikelet_path, error)),
            };
        }
    };

    if output.status.success() && output.stdout.is_empty() && output.stderr.is_empty() {
        Outcome::Passed
    } else {
        let mut msg = String::new();

        writeln!(msg).unwrap();
        writeln!(msg, "Errors encountered:").unwrap();
        writeln!(msg).unwrap();
        if !output.status.success() {
            writeln!(msg, "  • Unexpected status").unwrap();
        }
        if !output.stdout.is_empty() {
            writeln!(msg, "  • Unexpected stdout").unwrap();
        }
        if !output.stderr.is_empty() {
            writeln!(msg, "  • Unexpected stderr").unwrap();
        }

        if !output.status.success() {
            writeln!(msg).unwrap();
            writeln!(msg, "---- {} status ----", test.name).unwrap();
            writeln!(msg, "{}", output.status).unwrap();
        }
        if !output.stdout.is_empty() {
            writeln!(msg).unwrap();
            writeln!(msg, "---- {} stdout ----", test.name).unwrap();
            msg.push_str(String::from_utf8_lossy(&output.stdout).trim_end());
        }
        if !output.stderr.is_empty() {
            writeln!(msg).unwrap();
            writeln!(msg, "---- {} stderr ----", test.name).unwrap();
            msg.push_str(String::from_utf8_lossy(&output.stderr).trim_end());
        }

        Outcome::Failed { msg: Some(msg) }
    }
}
