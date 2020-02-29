use logos::Logos;
use std::convert::TryFrom;
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
    #[token = "λ"]
    Fun,
    #[token = "=>"]
    #[token = "⇒"]
    DArrow,
    #[token = "->"]
    #[token = "→"]
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

// Not really sure whether try_from is the best way to do this.
// In particular this only succeeds for the nullary Tokens.
impl<'a> TryFrom<&LexToken> for Token<'a> {
    type Error = &'static str;
    fn try_from(t: &LexToken) -> Result<Self, Self::Error> {
        use LexToken as T;
        match &t {
            T::Colon => Ok(Self::Colon),
            T::Comma => Ok(Self::Comma),
            T::Fun => Ok(Self::Fun),
            T::DArrow => Ok(Self::DArrow),
            T::Arrow => Ok(Self::Arrow),
            T::LParen => Ok(Self::LParen),
            T::RParen => Ok(Self::RParen),
            T::LBrack => Ok(Self::LBrack),
            T::RBrack => Ok(Self::RBrack),
            T::LBrace => Ok(Self::LBrace),
            T::RBrace => Ok(Self::RBrace),
            T::RecordTerm => Ok(Self::RecordTerm),
            T::RecordType => Ok(Self::RecordType),
            T::Dot => Ok(Self::Dot),
            T::Equal => Ok(Self::Equal),
            t => {
                println!("{:?}", t);
                Err("Error in converting LexicalToken to Token")
            }
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
        let mut lex;
        let mut range;
        loop {
            lex = &mut self.0;
            range = lex.range();
            match &lex.token {
                LexToken::Whitespace => {
                    // Skip whitespace
                    lex.advance();
                    continue;
                }
                _t => break,
            }
        }

        let it = match &lex.token {
            LexToken::EOF => None,
            // First match all the Token members with parameters.
            // Think as-is this includes single quotes
            LexToken::CharLit => Some(Ok((range.start, Token::CharLit(lex.slice()), range.end))),
            LexToken::Name => Some(Ok((range.start, Token::Name(lex.slice()), range.end))),
            // We could do something here besides expose this as a string if desired,
            // that doesn't work with the way that Literal is though...
            LexToken::Number => Some(Ok((range.start, Token::Number(lex.slice()), range.end))),
            // Think as-is this includes double quotes.
            LexToken::StrLit => Some(Ok((range.start, Token::StrLit(lex.slice()), range.end))),
            LexToken::Shift => Some(Ok((range.start, Token::Shift(lex.slice()), range.end))),
            LexToken::Error => Some(Err(LexicalError(range, "Error token encountered"))),
            // Lastly convert all the unitary LexTokens.
            t => {
                let t = Token::try_from(t);
                match t {
                    Ok(t) => Some(Ok((range.start, t, range.end))),
                    Err(e) => Some(Err(LexicalError(range, e))),
                }
            }
        };
        lex.advance();
        it
    }
}
