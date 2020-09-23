use logos::Logos;
use std::fmt;

use crate::reporting::LexerError;

/// Tokens in the surface language.
#[derive(Debug, Clone, Logos)]
pub enum Token<'a> {
    #[regex(r"\|\|\|(.*)\n")]
    DocComment(&'a str),
    #[regex(r#"'([^'\\]|\\.)*'"#)]
    CharLiteral(&'a str),
    #[regex(r#""([^"\\]|\\.)*""#)] // workaround editor highlighting: "
    StringLiteral(&'a str),
    #[regex(r"[-+]?[0-9][a-zA-Z0-9_\.]*")]
    NumericLiteral(&'a str),
    #[regex(r"[a-zA-Z][a-zA-Z0-9\-]*")]
    Name(&'a str),
    #[regex(r"\^[0-9]+(\.[0-9]+)?")]
    Shift(&'a str),

    #[token("as")]
    As,
    #[token("fun")]
    FunTerm,
    #[token("Fun")]
    FunType,
    #[token("record")]
    RecordTerm,
    #[token("Record")]
    RecordType,

    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("=>")]
    DArrow,
    #[token("->")]
    Arrow,
    #[token(".")]
    Dot,
    #[token("=")]
    Equal,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    #[error]
    #[regex(r"\p{Whitespace}", logos::skip)]
    #[regex(r"--(.*)\n", logos::skip)]
    Error,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::DocComment(s) => write!(f, "{}", s),
            Token::CharLiteral(s) => write!(f, "{}", s),
            Token::StringLiteral(s) => write!(f, "{}", s),
            Token::NumericLiteral(s) => write!(f, "{}", s),
            Token::Name(s) => write!(f, "{}", s),
            Token::Shift(s) => write!(f, "{}", s),

            Token::As => write!(f, "as"),
            Token::FunTerm => write!(f, "fun"),
            Token::FunType => write!(f, "Fun"),
            Token::RecordTerm => write!(f, "record"),
            Token::RecordType => write!(f, "Record"),

            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::DArrow => write!(f, "=>"),
            Token::Arrow => write!(f, "->"),
            Token::Equal => write!(f, "="),
            Token::Dot => write!(f, "."),

            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrack => write!(f, "["),
            Token::RBrack => write!(f, "]"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),

            Token::Error => write!(f, "<error>"),
        }
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub fn tokens<'a>(
    source: &'a str,
) -> impl 'a + Iterator<Item = Spanned<Token<'a>, usize, LexerError>> {
    Token::lexer(source)
        .spanned()
        .map(|(token, range)| match token {
            Token::Error => Err(LexerError::InvalidToken { range }),
            token => Ok((range.start, token, range.end)),
        })
}

#[test]
fn behavior_after_error() {
    let starts_with_invalid = "@.";
    // [Err(...), Some(Token::DOT)]
    let from_lex: Vec<_> = tokens(starts_with_invalid).collect();
    let result: Vec<_> = from_lex.iter().map(Result::is_ok).collect();
    assert_eq!(result, vec![false, true]);
}
