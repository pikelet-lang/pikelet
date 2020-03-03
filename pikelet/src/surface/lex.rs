use logos::Logos;
use std::fmt;

/// The complete set of `LexToken`s some of which never escape the lexer.
/// See Token for a list of which Tokens do and do not escape.
#[derive(Logos)]
enum LexToken {
    #[end]
    EOF,
    #[token = ":"]
    Colon,
    #[token = ","]
    Comma,
    #[token = "fun"]
    Fun,
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

/// The subset of `LexToken`s for the parser.
/// The tokens in `LexToken`s which are excluded from this enum are:
/// * Whitespace -- skipped.
/// * EOF -- turned into None, rather than a token.
/// * Error -- turned into Some(Err(...)).
#[derive(Debug, Clone)]
pub enum Token<'a> {
    Colon,
    Comma,
    Fun,
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
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::Fun => write!(f, "Fun"),
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
            Token::CharLiteral(s) => write!(f, "CharLiteral({})", s),
            Token::StrLiteral(s) => write!(f, "StrLiteral({})", s),
            Token::NumLiteral(s) => write!(f, "NumLiteral({})", s),
            Token::Name(s) => write!(f, "Write({})", s),
            Token::Shift(s) => write!(f, "Shift({})", s),
        }
    }
}

#[derive(Debug)]
pub struct LexicalError(std::ops::Range<usize>, &'static str);

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Lexical error: {:?} {}", self.0, self.1)
    }
}

pub struct Tokens<'a>(logos::Lexer<LexToken, &'a str>);
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'a> Tokens<'a> {
    pub fn new(source: &'a str) -> Tokens<'a> {
        Tokens(LexToken::lexer(source))
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = Spanned<Token<'a>, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let lex = &mut self.0;

        const fn tok<'a>(
            r: std::ops::Range<usize>,
            t: Token<'a>,
        ) -> Option<Spanned<Token<'a>, usize, LexicalError>> {
            Some(Ok((r.start, t, r.end)))
        }

        let range = lex.range();

        let token = loop {
            match &lex.token {
                // There doesn't seem to be any harm in advancing after EOF.
                // But we might as well return.
                LexToken::EOF => return None,
                LexToken::Error => break Some(Err(LexicalError(range, "Lexical error"))),
                LexToken::Whitespace => {
                    lex.advance();
                    continue;
                }
                LexToken::Colon => break tok(range, Token::Colon),
                LexToken::Comma => break tok(range, Token::Comma),
                LexToken::Fun => break tok(range, Token::Fun),
                LexToken::DArrow => break tok(range, Token::DArrow),
                LexToken::Arrow => break tok(range, Token::Arrow),
                LexToken::LParen => break tok(range, Token::LParen),
                LexToken::RParen => break tok(range, Token::RParen),
                LexToken::LBrack => break tok(range, Token::LBrack),
                LexToken::RBrack => break tok(range, Token::RBrack),
                LexToken::LBrace => break tok(range, Token::LBrace),
                LexToken::RBrace => break tok(range, Token::RBrace),
                LexToken::Dot => break tok(range, Token::Dot),
                LexToken::Equal => break tok(range, Token::Equal),
                LexToken::RecordTerm => break tok(range, Token::RecordTerm),
                LexToken::RecordType => break tok(range, Token::RecordType),
                LexToken::Name => break tok(range, Token::Name(lex.slice())),
                LexToken::Shift => break tok(range, Token::Shift(lex.slice())),
                LexToken::NumLiteral => break tok(range, Token::NumLiteral(lex.slice())),
                LexToken::CharLiteral => break tok(range, Token::CharLiteral(lex.slice())),
                LexToken::StrLiteral => break tok(range, Token::StrLiteral(lex.slice())),
            }
        };
        lex.advance();
        token
    }
}

#[test]
fn behavior_after_error() {
    let starts_with_invalid = "@.";
    // [Err(...), Some(Token::DOT)]
    let from_lex: Vec<Spanned<Token<'static>, usize, LexicalError>> =
        Tokens::new(starts_with_invalid).collect();
    let result: Vec<bool> = from_lex.iter().map(Result::is_ok).collect();
    assert_eq!(result, vec![false, true]);
}
