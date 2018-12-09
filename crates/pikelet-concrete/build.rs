fn main() {
    lalrpop::Configuration::new()
        .always_use_colors()
        .use_cargo_dir_conventions()
        .process()
        .unwrap();
}
