use logos::Logos;
use std::fmt;
use std::ops::Range;

/// The complete set of `LexerToken`s some of which never escape the lexer.
/// See `Token` for a list of which Tokens do and do not escape.
#[derive(Logos)]
enum LexerToken {
    #[end]
    Eof,
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
    #[token = r"record"]
    RecordTerm,
    #[token = r"Record"]
    RecordType,
    #[token = "."]
    Dot,
    #[token = "="]
    Equal,
    #[regex = r"\-\-(.*)\n"]
    Comment,
    // Hmm, not sure why this doesn't work.
    // #[regex = r#"'(.|\\"|\\')*'"#]
    #[regex = r#"('[^'\\]|\\t|\\u|\\n|\\"|\\')*'"#]
    CharLiteral,
    // Ditto.
    // #[regex = r#""(.|\\"|\\')*""#]
    #[regex = r#""([^"\\]|\\t|\\u|\\n|\\"|\\')*""#]
    StrLiteral,
    #[regex = r"[-+]?[0-9]+(\.[0-9]+)?"]
    NumLiteral,
    #[regex = r"[a-zA-Z][a-zA-Z0-9\-]*"]
    Name,
    #[regex = r"\^[0-9]+(\.[0-9]+)?"]
    Shift,
    #[regex = r"\p{Whitespace}"]
    Whitespace,
    #[error]
    Error,
}

/// The subset of `LexerToken`s for the parser.
///
/// The tokens in `LexerToken`s which are excluded from this enum are:
///
/// - `Whitespace`: skipped.
/// - `Eof`: turned into `None`, rather than a token.
/// - `Error`: turned into `Some(Err(...))`.
///
/// Comment while a valid token has been reserved but is not currently
/// emitted by the lexer.
#[derive(Debug, Clone)]
pub enum Token<'a> {
    Colon,
    Comma,
    FunTerm,
    DArrow,
    Arrow,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    RecordTerm,
    RecordType,
    Equal,
    Dot,
    CharLiteral(&'a str),
    StrLiteral(&'a str),
    NumLiteral(&'a str),
    Name(&'a str),
    Shift(&'a str),
    // Not in use at this time.
    Comment(&'a str),
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
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
            Token::Comment(s) => write!(f, "Comment({})", s),
            Token::CharLiteral(s) => write!(f, "CharLiteral({})", s),
            Token::StrLiteral(s) => write!(f, "StrLiteral({})", s),
            Token::NumLiteral(s) => write!(f, "NumLiteral({})", s),
            Token::Name(s) => write!(f, "Write({})", s),
            Token::Shift(s) => write!(f, "Shift({})", s),
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

pub struct Tokens<'a> {
    lexer: logos::Lexer<LexerToken, &'a str>,
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'a> Tokens<'a> {
    pub fn new(source: &'a str) -> Tokens<'a> {
        Tokens {
            lexer: LexerToken::lexer(source),
        }
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Spanned<Token<'a>, usize, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let lexer = &mut self.lexer;

        const fn tok<'a>(
            r: Range<usize>,
            t: Token<'a>,
        ) -> Option<Spanned<Token<'a>, usize, LexerError>> {
            Some(Ok((r.start, t, r.end)))
        }

        let range = lexer.range();

        let token = loop {
            match &lexer.token {
                // There doesn't seem to be any harm in advancing after `Eof`.
                // But we might as well return.
                LexerToken::Eof => return None,
                LexerToken::Error => break Some(Err(LexerError::InvalidToken(range))),
                LexerToken::Whitespace | LexerToken::Comment => {
                    lexer.advance();
                    continue;
                }
                LexerToken::Colon => break tok(range, Token::Colon),
                LexerToken::Comma => break tok(range, Token::Comma),
                LexerToken::FunTerm => break tok(range, Token::FunTerm),
                LexerToken::DArrow => break tok(range, Token::DArrow),
                LexerToken::Arrow => break tok(range, Token::Arrow),
                LexerToken::LParen => break tok(range, Token::LParen),
                LexerToken::RParen => break tok(range, Token::RParen),
                LexerToken::LBrack => break tok(range, Token::LBrack),
                LexerToken::RBrack => break tok(range, Token::RBrack),
                LexerToken::LBrace => break tok(range, Token::LBrace),
                LexerToken::RBrace => break tok(range, Token::RBrace),
                LexerToken::Dot => break tok(range, Token::Dot),
                LexerToken::Equal => break tok(range, Token::Equal),
                LexerToken::RecordTerm => break tok(range, Token::RecordTerm),
                LexerToken::RecordType => break tok(range, Token::RecordType),
                LexerToken::Name => break tok(range, Token::Name(lexer.slice())),
                LexerToken::Shift => break tok(range, Token::Shift(lexer.slice())),
                LexerToken::NumLiteral => break tok(range, Token::NumLiteral(lexer.slice())),
                LexerToken::CharLiteral => break tok(range, Token::CharLiteral(lexer.slice())),
                LexerToken::StrLiteral => break tok(range, Token::StrLiteral(lexer.slice())),
            }
        };
        lexer.advance();
        token
    }
}

#[test]
fn behavior_after_error() {
    let starts_with_invalid = "@.";
    // [Err(...), Some(Token::DOT)]
    let from_lex: Vec<Spanned<Token<'static>, usize, LexerError>> =
        Tokens::new(starts_with_invalid).collect();
    let result: Vec<bool> = from_lex.iter().map(Result::is_ok).collect();
    assert_eq!(result, vec![false, true]);
}
