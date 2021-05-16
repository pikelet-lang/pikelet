fn main() {
    let args = libtest_mimic::Arguments::from_args();

    std::env::set_current_dir("..").unwrap();

    let tests = std::iter::empty()
        .chain(pikelet_test::walk_files("examples").filter_map(pikelet_test::extract_simple_test))
        .chain(pikelet_test::walk_files("tests").filter_map(pikelet_test::extract_config_test))
        .collect();
    let run_test = pikelet_test::run_test(env!("CARGO_BIN_EXE_pikelet"));

    libtest_mimic::run_tests(&args, tests, run_test).exit();
}
