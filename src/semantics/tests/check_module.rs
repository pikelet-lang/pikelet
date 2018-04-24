use library;

use super::*;

#[test]
fn check_prelude() {
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), library::PRELUDE.into());
    let writer = StandardStream::stdout(ColorChoice::Always);

    let (concrete_module, errors) = parse::module(&filemap);
    if !errors.is_empty() {
        for error in errors {
            codespan_reporting::emit(&mut writer.lock(), &codemap, &error.to_diagnostic()).unwrap();
        }
        panic!("parse error!")
    }

    let module = concrete_module.to_core();
    if let Err(err) = check_module(&module) {
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}
