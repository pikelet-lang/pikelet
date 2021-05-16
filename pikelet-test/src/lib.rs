use libtest_mimic::{Outcome, Test};
use serde::Deserialize;
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

/// Extract a new simple test from the given file path
pub fn extract_simple_test(path: PathBuf) -> Option<Test<TestData>> {
    is_pikelet_path(&path).then(|| Test {
        name: path.display().to_string(),
        kind: String::new(),
        is_ignored: false,
        is_bench: false,
        data: TestData {
            input_file: path,
            parse_config: false,
        },
    })
}

/// Extract a new test configured with TOML-formatted comments from the given file path
pub fn extract_config_test(path: PathBuf) -> Option<Test<TestData>> {
    is_pikelet_path(&path).then(|| Test {
        name: path.display().to_string(),
        kind: String::new(),
        is_ignored: false,
        is_bench: false,
        data: TestData {
            input_file: path,
            parse_config: true,
        },
    })
}

/// Create a test runner function using the given Pikelet executable
pub fn run_test(
    pikelet_exe: &'static str,
) -> impl Fn(&Test<TestData>) -> Outcome + 'static + Send + Sync {
    move |test| run_test_impl(pikelet_exe, test)
}

fn run_test_impl(pikelet_exe: &str, test: &Test<TestData>) -> Outcome {
    let mut failures = Vec::new();

    let config = if test.data.parse_config {
        use itertools::Itertools;

        const CONFIG_COMMENT_START: &str = "--!";

        let input_source = fs::read_to_string(&test.data.input_file).unwrap();
        let config_source = input_source
            .lines()
            .filter_map(|line| line.split(CONFIG_COMMENT_START).nth(1))
            .join("\n");

        match toml::from_str(&config_source) {
            Ok(config) => config,
            Err(error) => {
                failures.push(Failure {
                    name: "config parse error".to_owned(),
                    details: error.to_string(),
                });

                return failures_to_outcome(&failures);
            }
        }
    } else {
        Config {
            ignore: false,
            check: CheckConfig {
                enable: true,
                validate_core: true,
            },
        }
    };

    if config.ignore || (!config.check.enable/* && ... */) {
        return Outcome::Ignored;
    }

    if config.check.enable {
        let output = Command::new(pikelet_exe)
            .arg("check")
            .arg("--validate-core")
            .arg(&test.data.input_file)
            .output();

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
    }

    failures_to_outcome(&failures)
}

pub struct TestData {
    input_file: PathBuf,
    parse_config: bool,
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "kebab-case")]
struct Config {
    #[serde(default = "false_value")]
    ignore: bool,
    #[serde(default)]
    check: CheckConfig,
}

#[derive(Deserialize, Debug)]
#[serde(deny_unknown_fields)]
#[serde(rename_all = "kebab-case")]
struct CheckConfig {
    #[serde(default = "false_value")]
    enable: bool,
    #[serde(default = "true_value")]
    validate_core: bool,
}

impl Default for CheckConfig {
    fn default() -> Self {
        CheckConfig {
            enable: false,
            validate_core: true,
        }
    }
}

fn false_value() -> bool {
    false
}

fn true_value() -> bool {
    false
}

struct Failure {
    name: String,
    details: String,
}

fn failures_to_outcome(failures: &[Failure]) -> Outcome {
    if failures.is_empty() {
        Outcome::Passed
    } else {
        let mut buffer = String::new();

        writeln!(buffer).unwrap();
        writeln!(buffer, "    failures:").unwrap();
        for failure in failures {
            writeln!(buffer).unwrap();
            writeln!(buffer, "    ---- {} ----", failure.name).unwrap();
            for line in failure.details.lines() {
                writeln!(buffer, "    {}", line).unwrap();
            }
        }
        writeln!(buffer).unwrap();
        writeln!(buffer).unwrap();
        writeln!(buffer, "    failures:").unwrap();
        for failure in failures {
            writeln!(buffer, "        {}", failure.name).unwrap();
        }

        Outcome::Failed { msg: Some(buffer) }
    }
}
