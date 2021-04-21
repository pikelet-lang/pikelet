fn main() -> Result<(), Box<dyn std::error::Error>> {
    lalrpop::Configuration::new()
        .always_use_colors()
        .process_current_dir()
}
