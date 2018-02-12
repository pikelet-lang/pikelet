use source::pos::Span;
use std::fmt;
use std::str::CharIndices;

use source::pos::BytePos;
use unicode_xid::UnicodeXID;

fn is_symbol(ch: char) -> bool {
    match ch {
        '&' | '!' | ':' | ',' | '.' | '=' | '/' | '>' | '<' | '-' | '|' | '+' | ';' | '*' => true,
        _ => false,
    }
}

fn is_ident_start(ch: char) -> bool {
    UnicodeXID::is_xid_start(ch) || ch == '_' || ch == '-'
}

fn is_ident_continue(ch: char) -> bool {
    UnicodeXID::is_xid_continue(ch) || ch == '_' || ch == '-'
}

fn is_dec_digit(ch: char) -> bool {
    ch.is_digit(10)
}

/// An error that occurred while lexing the source file
#[derive(Fail, Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    #[fail(display = "An unexpected character {:?} was found at byte {}.", found, start)]
    UnexpectedCharacter { start: BytePos, found: char },
}

impl LexerError {
    pub fn span(&self) -> Span {
        match *self {
            LexerError::UnexpectedCharacter { start, found } => Span::from_char_utf8(start, found),
        }
    }
}

/// A token in the source file, to be emitted by the `Lexer`
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token<S> {
    // Data
    Ident(S),
    DocComment(S),
    ReplCommand(S),
    DecLiteral(u64),

    // Keywords
    As,     // as
    Module, // module
    Import, // import
    Type,   // Type

    // Symbols
    BSlash,    // \
    Colon,     // :
    Comma,     // ,
    DotDot,    // ..
    Equal,     // =
    LArrow,    // ->
    LFatArrow, // =>
    Semi,      // ;

    // Delimiters
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]
}

impl<S: fmt::Display> fmt::Display for Token<S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Ident(ref name) => write!(f, "{}", name),
            Token::DocComment(ref comment) => write!(f, "||| {}", comment),
            Token::ReplCommand(ref command) => write!(f, ":{}", command),
            Token::DecLiteral(value) => write!(f, "{}", value),
            Token::As => write!(f, "as"),
            Token::Module => write!(f, "module"),
            Token::Import => write!(f, "import"),
            Token::Type => write!(f, "Type"),
            Token::BSlash => write!(f, "\\"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::DotDot => write!(f, ".."),
            Token::Equal => write!(f, "="),
            Token::LFatArrow => write!(f, "=>"),
            Token::LArrow => write!(f, "->"),
            Token::Semi => write!(f, ";"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
        }
    }
}

impl<'src> From<Token<&'src str>> for Token<String> {
    fn from(src: Token<&'src str>) -> Token<String> {
        match src {
            Token::Ident(name) => Token::Ident(String::from(name)),
            Token::DocComment(comment) => Token::DocComment(String::from(comment)),
            Token::ReplCommand(command) => Token::ReplCommand(String::from(command)),
            Token::DecLiteral(value) => Token::DecLiteral(value),
            Token::As => Token::As,
            Token::Module => Token::Module,
            Token::Import => Token::Import,
            Token::Type => Token::Type,
            Token::BSlash => Token::BSlash,
            Token::Colon => Token::Colon,
            Token::Comma => Token::Comma,
            Token::DotDot => Token::DotDot,
            Token::Equal => Token::Equal,
            Token::LFatArrow => Token::LFatArrow,
            Token::LArrow => Token::LArrow,
            Token::Semi => Token::Semi,
            Token::LParen => Token::LParen,
            Token::RParen => Token::RParen,
            Token::LBrace => Token::LBrace,
            Token::RBrace => Token::RBrace,
            Token::LBracket => Token::LBracket,
            Token::RBracket => Token::RBracket,
        }
    }
}

/// An iterator over a source string that yeilds `Token`s for subsequent use by
/// the parser
pub struct Lexer<'src> {
    src: &'src str,
    chars: CharIndices<'src>,
    lookahead: Option<(usize, char)>,
}

impl<'src> Lexer<'src> {
    /// Create a new lexer from the source string
    pub fn new(src: &'src str) -> Self {
        let mut chars = src.char_indices();

        Lexer {
            src,
            lookahead: chars.next(),
            chars,
        }
    }

    /// Return the next character in the source string
    fn lookahead(&self) -> Option<(BytePos, char)> {
        self.lookahead.map(|(index, ch)| (BytePos(index), ch))
    }

    /// Bump the current position in the source string by one character,
    /// returning the current character and byte position.
    fn bump(&mut self) -> Option<(BytePos, char)> {
        let current = self.lookahead();
        self.lookahead = self.chars.next();
        current
    }

    /// Return a slice of the source string
    fn slice(&self, start: BytePos, end: BytePos) -> &'src str {
        &self.src[start.0..end.0]
    }

    // /// Test a predicate againt the next character in the source
    // fn test_lookahead<F>(&self, mut pred: F) -> bool
    // where
    //     F: FnMut(char) -> bool,
    // {
    //     self.lookahead.map_or(false, |(_, ch)| pred(ch))
    // }

    /// Consume characters while the predicate matches for the current
    /// character, then return the consumed slice and the end byte
    /// position.
    fn take_while<F>(&mut self, start: BytePos, mut keep_going: F) -> (BytePos, &'src str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |ch| !keep_going(ch))
    }

    /// Consume characters until the predicate matches for the next character
    /// in the lookahead, then return the consumed slice and the end byte
    /// position.
    fn take_until<F>(&mut self, start: BytePos, mut terminate: F) -> (BytePos, &'src str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.lookahead() {
            if terminate(ch) {
                return (end, self.slice(start, end));
            } else {
                self.bump();
            }
        }

        let eof = BytePos(self.src.len());
        (eof, self.slice(start, eof))
    }

    /// Consume a REPL command
    fn repl_command(&mut self, start: BytePos) -> (BytePos, Token<&'src str>, BytePos) {
        let (end, command) = self.take_while(start.map(|x| x + 1), |ch| {
            is_ident_continue(ch) || ch == '?'
        });

        if command.is_empty() {
            (start, Token::Colon, end)
        } else {
            (start, Token::ReplCommand(command), end)
        }
    }

    /// Consume a doc comment
    fn doc_comment(&mut self, start: BytePos) -> (BytePos, Token<&'src str>, BytePos) {
        let (end, mut comment) = self.take_until(start.map(|x| x + 3), |ch| ch == '\n');

        // Skip preceding space
        if comment.starts_with(' ') {
            comment = &comment[1..];
        }

        (start, Token::DocComment(comment), end)
    }

    /// Consume an identifier token
    fn ident(&mut self, start: BytePos) -> (BytePos, Token<&'src str>, BytePos) {
        let (end, ident) = self.take_while(start, is_ident_continue);

        let token = match ident {
            "as" => Token::As,
            "module" => Token::Module,
            "import" => Token::Import,
            "Type" => Token::Type,
            ident => Token::Ident(ident),
        };

        (start, token, end)
    }

    /// Consume a decimal literal token
    fn dec_literal(&mut self, start: BytePos) -> (BytePos, Token<&'src str>, BytePos) {
        let (end, src) = self.take_while(start, is_dec_digit);

        let int = u64::from_str_radix(src, 10).unwrap();

        (start, Token::DecLiteral(int), end)
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<(BytePos, Token<&'src str>, BytePos), LexerError>;

    fn next(&mut self) -> Option<Result<(BytePos, Token<&'src str>, BytePos), LexerError>> {
        while let Some((start, ch)) = self.bump() {
            let end = start.map(|x| x + 1);

            return Some(match ch {
                ch if is_symbol(ch) => {
                    let (end, symbol) = self.take_while(start, is_symbol);

                    match symbol {
                        ":" => Ok(self.repl_command(start)),
                        "," => Ok((start, Token::Comma, end)),
                        ".." => Ok((start, Token::DotDot, end)),
                        "=" => Ok((start, Token::Equal, end)),
                        "->" => Ok((start, Token::LArrow, end)),
                        "=>" => Ok((start, Token::LFatArrow, end)),
                        ";" => Ok((start, Token::Semi, end)),
                        symbol if symbol.starts_with("|||") => Ok(self.doc_comment(start)),
                        symbol if symbol.starts_with("--") => {
                            self.take_until(start, |ch| ch == '\n');
                            continue;
                        },
                        _ => Err(LexerError::UnexpectedCharacter { start, found: ch }),
                    }
                },
                '\\' => Ok((start, Token::BSlash, end)),
                '(' => Ok((start, Token::LParen, end)),
                ')' => Ok((start, Token::RParen, end)),
                '{' => Ok((start, Token::LBrace, end)),
                '}' => Ok((start, Token::RBrace, end)),
                '[' => Ok((start, Token::LBracket, end)),
                ']' => Ok((start, Token::RBracket, end)),
                ch if is_ident_start(ch) => Ok(self.ident(start)),
                ch if is_dec_digit(ch) => Ok(self.dec_literal(start)),
                ch if ch.is_whitespace() => continue,
                _ => Err(LexerError::UnexpectedCharacter { start, found: ch }),
            });
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A handy macro to give us a nice syntax for declaring test cases
    ///
    /// This was inspired by the tests in the LALRPOP lexer
    macro_rules! test {
        ($src:expr, $($span:expr => $token:expr,)*) => {{
            let lexed_tokens: Vec<_> = Lexer::new($src).collect();
            let expected_tokens = vec![$({
                let start = BytePos($span.find("~").unwrap());
                let end = BytePos($span.rfind("~").unwrap() + 1);
                Ok((start, $token, end))
            }),*];

            assert_eq!(lexed_tokens, expected_tokens);
        }};
    }

    #[test]
    fn data() {
        test! {
            "  hello-hahaha8ABC  ",
            "  ~~~~~~~~~~~~~~~~  " => Token::Ident("hello-hahaha8ABC"),
        };
    }

    #[test]
    fn doc_comment() {
        test! {
            "       ||| hello this is dog",
            "       ~~~~~~~~~~~~~~~~~~~~~" => Token::DocComment("hello this is dog"),
        };
    }

    #[test]
    fn keywords() {
        test! {
            "  as module import Type  ",
            "  ~~                     " => Token::As,
            "     ~~~~~~              " => Token::Module,
            "            ~~~~~~       " => Token::Import,
            "                   ~~~~  " => Token::Type,
        };
    }

    #[test]
    fn symbols() {
        test! {
            r" \ : , .. = -> => ; ",
            r" ~                  " => Token::BSlash,
            r"   ~                " => Token::Colon,
            r"     ~              " => Token::Comma,
            r"       ~~           " => Token::DotDot,
            r"          ~         " => Token::Equal,
            r"            ~~      " => Token::LArrow,
            r"               ~~   " => Token::LFatArrow,
            r"                  ~ " => Token::Semi,
        }
    }

    #[test]
    fn delimiters() {
        test! {
            " ( ) { } [ ] ",
            " ~           " => Token::LParen,
            "   ~         " => Token::RParen,
            "     ~       " => Token::LBrace,
            "       ~     " => Token::RBrace,
            "         ~   " => Token::LBracket,
            "           ~ " => Token::RBracket,
        }
    }
}
