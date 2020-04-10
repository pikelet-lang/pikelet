use logos::Logos;
use std::fmt;
use std::ops::Range;

/// Tokens in the surface language.
#[derive(Debug, Clone, Logos)]
#[logos(trivia = r"\p{Whitespace}|--(.*)\n")]
pub enum Token<'a> {
    #[regex(r"\|\|\|(.*)\n", |lexer| lexer.slice())]
    DocComment(&'a str),
    #[regex(r#"('[^'\\]|\\')*'"#, |lexer| lexer.slice())]
    CharLiteral(&'a str),
    #[regex(r#""([^"\\]|\\")*""#, |lexer| lexer.slice())]
    StrLiteral(&'a str),
    #[regex(r"[-+]?[0-9]+(\.[0-9]+)?", |lexer| lexer.slice())]
    NumLiteral(&'a str),
    #[regex(r"[a-zA-Z][a-zA-Z0-9\-]*", |lexer| lexer.slice())]
    Name(&'a str),
    #[regex(r"\^[0-9]+(\.[0-9]+)?", |lexer| lexer.slice())]
    Shift(&'a str),
    #[token = ":"]
    Colon,
    #[token = ","]
    Comma,
    #[token = "fun"]
    FunTerm,
    #[token = "=>"]
    DArrow,
    #[token = "->"]
    Arrow,
    #[token = "("]
    LParen,
    #[token = ")"]
    RParen,
    #[token = "["]
    LBrack,
    #[token = "]"]
    RBrack,
    #[token = "{"]
    LBrace,
    #[token = "}"]
    RBrace,
    #[token = "record"]
    RecordTerm,
    #[token = "Record"]
    RecordType,
    #[token = "."]
    Dot,
    #[token = "="]
    Equal,
    #[error]
    Error,
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::DocComment(s) => write!(f, "{}", s),
            Token::CharLiteral(s) => write!(f, "{}", s),
            Token::StrLiteral(s) => write!(f, "{}", s),
            Token::NumLiteral(s) => write!(f, "{}", s),
            Token::Name(s) => write!(f, "{}", s),
            Token::Shift(s) => write!(f, "{}", s),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::FunTerm => write!(f, "fun"),
            Token::DArrow => write!(f, "=>"),
            Token::Arrow => write!(f, "->"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrack => write!(f, "["),
            Token::RBrack => write!(f, "]"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::RecordTerm => write!(f, "record"),
            Token::RecordType => write!(f, "Record"),
            Token::Equal => write!(f, "="),
            Token::Dot => write!(f, "."),
            Token::Error => write!(f, "<error>"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LexerError {
    InvalidToken(Range<usize>),
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LexerError::InvalidToken(range) => write!(f, "Invalid token: {:?}", range),
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
            Token::Error => Err(LexerError::InvalidToken(range)),
            token => Ok((range.start, token, range.end)),
        })
}

#[test]
fn behavior_after_error() {
    let starts_with_invalid = "@.";
    // [Err(...), Some(Token::DOT)]
    let from_lex: Vec<Spanned<Token<'static>, usize, LexerError>> =
        tokens(starts_with_invalid).collect();
    let result: Vec<bool> = from_lex.iter().map(Result::is_ok).collect();
    assert_eq!(result, vec![false, true]);
}
