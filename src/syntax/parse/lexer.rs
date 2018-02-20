use codespan::{ByteSpan, FileMap};
use codespan_reporting::{Diagnostic, Label, LabelStyle, Severity};
use std::fmt;
use std::str::CharIndices;

use codespan::{ByteIndex, ByteOffset, RawOffset};
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
    UnexpectedCharacter { start: ByteIndex, found: char },
}

impl LexerError {
    /// Return the span of source code that this error originated from
    pub fn span(&self) -> ByteSpan {
        match *self {
            LexerError::UnexpectedCharacter { start, found } => {
                ByteSpan::from_offset(start, ByteOffset::from_char_utf8(found))
            },
        }
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        match *self {
            LexerError::UnexpectedCharacter { start, found } => Diagnostic {
                severity: Severity::Error,
                message: format!("unexpected character {:?}", found),
                labels: vec![
                    Label {
                        message: None, // TODO
                        style: LabelStyle::Primary,
                        span: ByteSpan::from_offset(start, ByteOffset::from_char_utf8(found)),
                    },
                ],
            },
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

impl<'input> From<Token<&'input str>> for Token<String> {
    fn from(src: Token<&'input str>) -> Token<String> {
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
pub struct Lexer<'input> {
    filemap: &'input FileMap,
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>,
}

impl<'input> Lexer<'input> {
    /// Create a new lexer from the source string
    pub fn new(filemap: &'input FileMap) -> Self {
        let mut chars = filemap.src().char_indices();

        Lexer {
            filemap,
            lookahead: chars.next(),
            chars,
        }
    }

    /// Return the next character in the source string
    fn lookahead(&self) -> Option<(ByteIndex, char)> {
        self.lookahead.map(|(index, ch)| {
            let off = ByteOffset(index as RawOffset);
            let index = self.filemap.span().start() + off;
            (index, ch)
        })
    }

    /// Bump the current position in the source string by one character,
    /// returning the current character and byte position.
    fn bump(&mut self) -> Option<(ByteIndex, char)> {
        let current = self.lookahead();
        self.lookahead = self.chars.next();
        current
    }

    /// Return a slice of the source string
    fn slice(&self, start: ByteIndex, end: ByteIndex) -> &'input str {
        &self.filemap.src_slice(ByteSpan::new(start, end)).unwrap()
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
    fn take_while<F>(&mut self, start: ByteIndex, mut keep_going: F) -> (ByteIndex, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |ch| !keep_going(ch))
    }

    /// Consume characters until the predicate matches for the next character
    /// in the lookahead, then return the consumed slice and the end byte
    /// position.
    fn take_until<F>(&mut self, start: ByteIndex, mut terminate: F) -> (ByteIndex, &'input str)
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

        let eof = self.filemap.span().end();
        (eof, self.slice(start, eof))
    }

    /// Consume a REPL command
    fn repl_command(&mut self, start: ByteIndex) -> (ByteIndex, Token<&'input str>, ByteIndex) {
        let (end, command) = self.take_while(start + ByteOffset::from_str(":"), |ch| {
            is_ident_continue(ch) || ch == '?'
        });

        if command.is_empty() {
            (start, Token::Colon, end)
        } else {
            (start, Token::ReplCommand(command), end)
        }
    }

    /// Consume a doc comment
    fn doc_comment(&mut self, start: ByteIndex) -> (ByteIndex, Token<&'input str>, ByteIndex) {
        let (end, mut comment) =
            self.take_until(start + ByteOffset::from_str("|||"), |ch| ch == '\n');

        // Skip preceding space
        if comment.starts_with(' ') {
            comment = &comment[1..];
        }

        (start, Token::DocComment(comment), end)
    }

    /// Consume an identifier token
    fn ident(&mut self, start: ByteIndex) -> (ByteIndex, Token<&'input str>, ByteIndex) {
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
    fn dec_literal(&mut self, start: ByteIndex) -> (ByteIndex, Token<&'input str>, ByteIndex) {
        let (end, src) = self.take_while(start, is_dec_digit);

        let int = u64::from_str_radix(src, 10).unwrap();

        (start, Token::DecLiteral(int), end)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(ByteIndex, Token<&'input str>, ByteIndex), LexerError>;

    fn next(&mut self) -> Option<Result<(ByteIndex, Token<&'input str>, ByteIndex), LexerError>> {
        while let Some((start, ch)) = self.bump() {
            let end = start + ByteOffset::from_char_utf8(ch);

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
    use codespan::{CodeMap, FileName};
    use codespan::RawIndex;

    use super::*;

    /// A handy macro to give us a nice syntax for declaring test cases
    ///
    /// This was inspired by the tests in the LALRPOP lexer
    macro_rules! test {
        ($src:expr, $($span:expr => $token:expr,)*) => {{
            let mut codemap = CodeMap::new();
            let filemap = codemap.add_filemap(FileName::virtual_("test"), $src.into());

            let lexed_tokens: Vec<_> = Lexer::new(&filemap).collect();
            let expected_tokens = vec![$({
                let start = ByteIndex($span.find("~").unwrap() as RawIndex + 1);
                let end = ByteIndex($span.rfind("~").unwrap() as RawIndex + 2);
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
