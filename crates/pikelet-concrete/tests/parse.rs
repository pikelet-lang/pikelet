use codespan::{ByteIndex, ByteSpan};
use codespan::{CodeMap, FileName};
use pretty_assertions::assert_eq;

use pikelet_concrete::parse::{self, LexerError, ParseError};
use pikelet_concrete::syntax::concrete;

#[test]
fn imports() {
    let src = r#"
            record {
                prims = import "prims.pi";
                prelude = import "prelude.pi";
            }
        "#;
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

    let parse_result = parse::term(&filemap);

    assert_eq!(
        parse_result.1,
        vec!["prims.pi".to_owned(), "prelude.pi".to_owned()],
    );
}

#[test]
fn fun_ty_bad_ident() {
    let src = "((x : Type) : Type) -> Type";
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

    let parse_result = parse::term(&filemap);

    assert_eq!(
        parse_result,
        (
            concrete::Term::Error(ByteSpan::new(ByteIndex(1), ByteIndex(28))),
            vec![],
            vec![ParseError::IdentifierExpectedInPiType {
                span: ByteSpan::new(ByteIndex(2), ByteIndex(12)),
            }],
        )
    );
}

#[test]
fn fun_ty_bad_ident_multi() {
    let src = "((x : Type) : Type) (x : Type) -> Type";
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

    let parse_result = parse::term(&filemap);

    assert_eq!(
        parse_result,
        (
            concrete::Term::Error(ByteSpan::new(ByteIndex(1), ByteIndex(39))),
            vec![],
            vec![ParseError::IdentifierExpectedInPiType {
                span: ByteSpan::new(ByteIndex(2), ByteIndex(12)),
            }],
        )
    );
}

#[test]
fn integer_overflow() {
    let src = "Type^111111111111111111111111111111";
    let mut codemap = CodeMap::new();
    let filemap = codemap.add_filemap(FileName::virtual_("test"), src.into());

    let parse_result = parse::term(&filemap);

    assert_eq!(
        parse_result,
        (
            concrete::Term::Error(ByteSpan::new(ByteIndex(1), ByteIndex(36))),
            vec![],
            vec![ParseError::Lexer(LexerError::IntegerLiteralOverflow {
                span: ByteSpan::new(ByteIndex(6), ByteIndex(36)),
                value: "111111111111111111111111111111".to_owned(),
            })],
        )
    );
}
