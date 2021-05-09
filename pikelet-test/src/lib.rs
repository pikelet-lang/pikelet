use libtest_mimic::{Outcome, Test};
use std::fmt::Write;
use std::fs;
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

pub struct TestData {
    pub input_file: PathBuf,
}

/// Extract a new test from the given file path
pub fn extract_test(path: PathBuf) -> Option<Test<TestData>> {
    is_pikelet_path(&path).then(|| Test {
        name: path.display().to_string(),
        kind: String::new(),
        is_ignored: false,
        is_bench: false,
        data: TestData { input_file: path },
    })
}

/// Create a test runner function using the given Pikelet executable
pub fn run_test(
    pikelet_exe: &'static str,
) -> impl Fn(&Test<TestData>) -> Outcome + 'static + Send + Sync {
    move |test| run_test_impl(pikelet_exe, test)
}

fn run_test_impl(pikelet_exe: &str, test: &Test<TestData>) -> Outcome {
    let input_source = fs::read_to_string(&test.data.input_file).unwrap();
    if input_source.starts_with("--! IGNORE") {
        return Outcome::Ignored;
    }

    let output = Command::new(pikelet_exe)
        .arg("check")
        .arg("--validate-core")
        .arg(&test.data.input_file)
        .output();

    let mut failures = Vec::new();

    match output {
        Ok(output) => {
            if !output.status.success() {
                failures.push(Failure {
                    name: "exit status".to_owned(),
                    details: output.status.to_string(),
                });
            }
            if !output.stdout.is_empty() {
                failures.push(Failure {
                    name: "stdout".to_owned(),
                    details: String::from_utf8_lossy(&output.stdout).into(),
                });
            }
            if !output.stderr.is_empty() {
                failures.push(Failure {
                    name: "stderr".to_owned(),
                    details: String::from_utf8_lossy(&output.stderr).into(),
                });
            }
        }
        Err(error) => failures.push(Failure {
            name: "command error".to_owned(),
            details: error.to_string(),
        }),
    }

    if failures.is_empty() {
        Outcome::Passed
    } else {
        let mut buffer = String::new();

        writeln!(buffer).unwrap();
        writeln!(
            buffer,
            "    {program} check --validate-core {input_file}",
            program = pikelet_exe,
            input_file = test.data.input_file.display(),
        )
        .unwrap();
        writeln!(buffer).unwrap();
        writeln!(buffer, "    failures:").unwrap();
        for failure in &failures {
            writeln!(buffer).unwrap();
            writeln!(buffer, "    ---- {} ----", failure.name).unwrap();
            for line in failure.details.lines() {
                writeln!(buffer, "    {}", line).unwrap();
            }
        }
        writeln!(buffer).unwrap();
        writeln!(buffer).unwrap();
        writeln!(buffer, "    failures:").unwrap();
        for failure in &failures {
            writeln!(buffer, "        {}", failure.name).unwrap();
        }

        Outcome::Failed { msg: Some(buffer) }
    }
}

struct Failure {
    name: String,
    details: String,
}
