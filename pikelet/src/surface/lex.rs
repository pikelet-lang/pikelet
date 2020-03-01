use logos::Logos;
use std::fmt::{self, Display, Formatter};

/// The complete set of `LexToken`s some of which never escape the lexer.
/// See Token for a list of which Tokens do and do not escape.
#[derive(Logos, Debug)]
pub enum LexToken {
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
    CharLit,
    // Ditto.
    // #[regex = r#""(.|\\"|\\')*""#]
    #[regex = r#""([^"\\]|\\t|\\u|\\n|\\"|\\')*""#]
    StrLit,
    #[regex = r"[-+]?[0-9]+(\.[0-9]+)?"]
    Number,
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
    CharLit(&'a str),
    StrLit(&'a str),
    Number(&'a str),
    Name(&'a str),
    Shift(&'a str),
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use Token as T;
        match self {
            T::Colon => write!(f, ":"),
            T::Comma => write!(f, ","),
            T::Fun => write!(f, "Fun"),
            T::DArrow => write!(f, "=>"),
            T::Arrow => write!(f, "->"),
            T::LParen => write!(f, "("),
            T::RParen => write!(f, ")"),
            T::LBrack => write!(f, "["),
            T::RBrack => write!(f, "]"),
            T::LBrace => write!(f, "{{"),
            T::RBrace => write!(f, "}}"),
            T::RecordTerm => write!(f, "record"),
            T::RecordType => write!(f, "Record"),
            T::Equal => write!(f, "="),
            T::Dot => write!(f, "."),
            T::CharLit(s) => write!(f, "CharLit({})", s),
            T::StrLit(s) => write!(f, "StrLit({})", s),
            T::Number(s) => write!(f, "Number({})", s),
            T::Name(s) => write!(f, "Write({})", s),
            T::Shift(s) => write!(f, "Shift({})", s),
        }
    }
}

#[derive(Debug)]
pub struct LexicalError(std::ops::Range<usize>, &'static str);

impl Display for LexicalError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Lexical error: {:?} {}", self.0, self.1)
    }
}

pub struct LexIterator<'a>(logos::Lexer<LexToken, &'a str>);
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

impl<'a> LexIterator<'a> {
    pub fn new(source: &'a str) -> LexIterator<'a> {
        LexIterator(LexToken::lexer(source))
    }
}

impl<'a> Iterator for LexIterator<'a> {
    type Item = Spanned<Token<'a>, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        use LexToken as LT;
        use Token as T;
        let lex = &mut self.0;
        loop {
            match &lex.token {
                LT::Whitespace => {
                    // Skip whitespace
                    lex.advance();
                    continue;
                }
                _ => break,
            }
        }

        const fn tok<'a>(
            r: std::ops::Range<usize>,
            t: Token<'a>,
        ) -> Option<Spanned<Token<'a>, usize, LexicalError>> {
            Some(Ok((r.start, t, r.end)))
        }

        let r = lex.range();
        // I'm really not fond of this, but trying to split this match statement up
        // appears to be more trouble than it is worth.
        #[rustfmt::skip]
        let it: Option<Self::Item> = match &lex.token {
            LT::EOF => None,
            LT::Error => Some(Err(LexicalError(r, "Lexical error"))),
            LT::Whitespace => unreachable!(),
            LT::Colon  => tok(r, T::Colon),
            LT::Comma  => tok(r, T::Comma),
            LT::Fun    => tok(r, T::Fun),
            LT::DArrow => tok(r, T::DArrow),
            LT::Arrow  => tok(r, T::Arrow),
            LT::LParen => tok(r, T::LParen),
            LT::RParen => tok(r, T::RParen),
            LT::LBrack => tok(r, T::LBrack),
            LT::RBrack => tok(r, T::RBrack),
            LT::LBrace => tok(r, T::LBrace),
            LT::RBrace => tok(r, T::RBrace),
            LT::Dot    => tok(r, T::Dot),
            LT::Equal  => tok(r, T::Equal),
            LT::RecordTerm => tok(r, T::RecordTerm),
            LT::RecordType => tok(r, T::RecordType),
            LT::Name    => tok(r, T::Name(lex.slice())),
            // We could do something here besides expose this as a string if desired,
            // that doesn't work with the way that Literal is though...
            LT::Number  => tok(r, T::Number(lex.slice())),
            LT::Shift   => tok(r, T::Shift(lex.slice())),
            // I think as-is the next 2 will include the pair of quotation marks.
            // These probably need to be stripped from CharLit and StrLit?
            LT::CharLit => tok(r, T::CharLit(lex.slice())),
            LT::StrLit  => tok(r, T::StrLit(lex.slice())),
        };
        lex.advance();
        it
    }
}
