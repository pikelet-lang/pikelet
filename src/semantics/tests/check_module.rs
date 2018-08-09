use library;

use super::*;

#[test]
fn prelude() {
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

    let raw_module = concrete_module.desugar(&DesugarEnv::new());
    if let Err(err) = check_module(&TcEnv::default(), &raw_module) {
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn infer_bare_definition() {
    let mut codemap = CodeMap::new();

    let src = "
        foo = true;
    ";

    let raw_module = parse_module(&mut codemap, src);
    if let Err(err) = check_module(&TcEnv::default(), &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn forward_decls() {
    let mut codemap = CodeMap::new();
    let writer = StandardStream::stdout(ColorChoice::Always);

    let src = "
        foo : Bool;
        bar : Bool;
        bar = true;
        foo = false;
    ";

    let raw_module = parse_module(&mut codemap, src);
    if let Err(err) = check_module(&TcEnv::default(), &raw_module) {
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn forward_decls_forward_ref() {
    let mut codemap = CodeMap::new();

    let src = "
        foo : Bool;
        bar : Bool;
        bar = foo;
        foo = false;
    ";

    let raw_module = parse_module(&mut codemap, src);
    match check_module(&TcEnv::default(), &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::NotYetDefined { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn claim_after_definition() {
    let mut codemap = CodeMap::new();

    let src = "
        foo = true;
        foo : Bool;
    ";

    let raw_module = parse_module(&mut codemap, src);
    match check_module(&TcEnv::default(), &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::ClaimFollowedDefinition { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn duplicate_claims() {
    let mut codemap = CodeMap::new();

    let src = "
        foo : Bool;
        foo : I32;
    ";

    let raw_module = parse_module(&mut codemap, src);
    match check_module(&TcEnv::default(), &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::DuplicateClaims { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn duplicate_definitions() {
    let mut codemap = CodeMap::new();

    let src = "
        foo = Bool;
        foo = I32;
    ";

    let raw_module = parse_module(&mut codemap, src);
    match check_module(&TcEnv::default(), &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::DuplicateDefinitions { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}
