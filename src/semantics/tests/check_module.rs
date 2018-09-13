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

#[test]
fn shift_universes() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = r#"
        id : (a : Type) -> a -> a;
        id a x = x;

        test1 = id String "hello";
        test2 = id I32 1;
        test3 = id^1 Type String;
        test4 = id^2 Type String;
        test5 = id^2 Type^1 String;
        test6 = id^2 Type^1 Type;

        id1 : (a : Type^1) -> a -> a = id^1;
        id2 : (a : Type^2) -> a -> a = id^2;
        id11 : (a : Type^2) -> a -> a = id1^1;
        id22 : (a : Type^4) -> a -> a = id2^2;
    "#;

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn shift_universes_id_self_application() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    // Here is a curious example from the Idris docs:
    // http://docs.idris-lang.org/en/v1.3.0/tutorial/miscellany.html#cumulativity
    //
    // ```idris
    // myid : (a : Type) -> a -> a
    // myid _ x = x
    //
    // idid : (a : Type) -> a -> a
    // idid = myid _ myid
    // ```
    //
    // This would cause a cycle in the universe hierarchy in Idris, but is
    // perfectly ok when implemented using explicit universe shifting.

    let src = r#"
        id : (a : Type) -> a -> a;
        id a x = x;

        id-id : (a : Type) -> a -> a;
        id-id = id^1 ((a : Type) -> a -> a) id;
    "#;

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn shift_universes_literals() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = r#"
        id : (a : Type) -> a -> a;
        id a x = x;

        test2 = "hello" : id^1 Type String;
    "#;

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}

#[test]
fn shift_universes_literals_bad() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = r#"
        id : (a : Type) -> a -> a;
        id a x = x;

        test2 = "hello" : id^2 Type^1 String^1;
    "#;

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::LiteralMismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn shift_universes_too_little() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = r#"
        id : (a : Type) -> a -> a;
        id a x = x;

        test1 = id^1 Type^1 Type;
    "#;

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    match check_module(&tc_env, &raw_module) {
        Ok(_) => panic!("expected error"),
        Err(TypeError::Mismatch { .. }) => {},
        Err(err) => panic!("unexpected error: {}", err),
    }
}

#[test]
fn shift_universes_too_much() {
    let mut codemap = CodeMap::new();
    let tc_env = TcEnv::default();
    let desugar_env = DesugarEnv::new(tc_env.mappings());

    let src = r#"
        id : (a : Type) -> a -> a;
        id a x = x;

        test1 = id^2 Type String;
    "#;

    let raw_module = parse_module(&mut codemap, src).desugar(&desugar_env);
    if let Err(err) = check_module(&tc_env, &raw_module) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        codespan_reporting::emit(&mut writer.lock(), &codemap, &err.to_diagnostic()).unwrap();
        panic!("type error!")
    }
}
