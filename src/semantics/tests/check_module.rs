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

    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());
    if let Err(err) = check_module(&tc_env, &concrete_module.desugar(&desugar_env)) {
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn infer_bare_definition() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        foo = true;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn forward_declarations() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        foo : Bool;
        bar : Bool;
        bar = true;
        foo = false;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn forward_declarations_forward_ref() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        foo : Bool;
        bar : Bool;
        bar = foo;
        foo = false;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::UndefinedName { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn declaration_after_definition() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        foo = true;
        foo : Bool;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::DeclarationFollowedDefinition { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn duplicate_declarations() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        foo : Bool;
        foo : I32;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::DuplicateDeclarations { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn duplicate_definitions() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = "
        foo = Bool;
        foo = I32;
    ";

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::DuplicateDefinitions { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}
