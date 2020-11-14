//! Decoding of [literals] in the surface language into Rust datatypes.
//!
//! [literals]: https://en.wikipedia.org/wiki/Literal_%28computer_programming%29

use crossbeam_channel::Sender;
use logos::Logos;
use num_traits::{Float, PrimInt, Signed, Unsigned};

use crate::lang::Range;
use crate::reporting::LiteralParseMessage::*;
use crate::reporting::Message;

/// The maximum character code permitted in Unicode escape sequences.
pub const MAX_UNICODE: u32 = 0x10FFFF;
/// The maximum character code permitted in ASCII escape sequences.
pub const MAX_ASCII: u32 = 0x7F;

/// The sign of a numeric literal.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Sign {
    Positive,
    Negative,
}

/// The [base] of a numeric digit.
///
/// [base]: https://en.wikipedia.org/wiki/Radix
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Base {
    Binary,
    Octal,
    Decimal,
    Hexadecimal,
}

impl Base {
    pub fn to_u8(self) -> u8 {
        match self {
            Base::Binary => 2,
            Base::Octal => 8,
            Base::Decimal => 10,
            Base::Hexadecimal => 16,
        }
    }
}

/// Convert the first byte of the source string to a digit.
fn ascii_digit<'source, Token>(lexer: &mut logos::Lexer<'source, Token>) -> Option<u8>
where
    Token: Logos<'source, Source = [u8]>,
{
    match lexer.slice().first()? {
        byte @ b'0'..=b'9' => Some(byte - b'0'),
        byte @ b'a'..=b'z' => Some(byte - b'a' + 10),
        byte @ b'A'..=b'Z' => Some(byte - b'A' + 10),
        _ => None,
    }
}

/// Numeric literal tokens.
#[derive(Debug, Clone, Logos)]
enum NumericLiteral {
    #[token(b"+", |_| Sign::Positive)]
    #[token(b"-", |_| Sign::Negative)]
    Sign(Sign),
    #[token(b"0b", |_| Base::Binary)]
    #[token(b"0o", |_| Base::Octal)]
    #[token(b"0x", |_| Base::Hexadecimal)]
    Base(Base),
    #[regex(b"[0-9]", ascii_digit)]
    Digit(u8),

    #[error]
    Error,
}

/// Digits up to base 32.
#[derive(Debug, Clone, Logos)]
enum Digit36 {
    #[regex(b"[0-9a-zA-Z]", ascii_digit)]
    Digit(u8),
    #[regex(b"_+")]
    Separator,

    #[error]
    Error,
}

/// Digits up to base 10.
#[derive(Debug, Clone, Logos)]
enum Digit10 {
    #[regex(b"[0-9]", ascii_digit)]
    Digit(u8),
    #[regex(b"_+")]
    Separator,
    #[token(b".")]
    StartFractional,
    #[token(b"e")]
    #[token(b"E")]
    StartExponent,

    #[error]
    Error,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Quote {
    Single,
    Double,
}

impl Quote {
    fn to_char(self) -> char {
        match self {
            Quote::Single => '\'',
            Quote::Double => '\"',
        }
    }
}

#[derive(Debug, Clone, Logos)]
enum QuotedLiteral {
    #[token("\'", |_| Quote::Single)]
    #[token("\"", |_| Quote::Double)]
    Start(Quote),

    #[error]
    Error,
}

#[derive(Debug, Clone, Logos)]
enum QuotedText<'source> {
    #[regex(r#"[^\\"']+"#)]
    Utf8Text(&'source str),
    #[token("\\")]
    StartEscape,
    #[token("\'", |_| Quote::Single)]
    #[token("\"", |_| Quote::Double)]
    End(Quote),

    #[error]
    Error,
}

#[derive(Debug, Clone, Logos)]
enum EscapeSequence {
    #[token("\\", |_| '\\')]
    #[token("n", |_| '\n')]
    #[token("r", |_| '\r')]
    #[token("t", |_| '\t')]
    #[token("0", |_| '\0')]
    #[token("\'", |_| '\'')]
    #[token("\"", |_| '\"')]
    Single(char),
    #[token("u")]
    StartUnicodeEscape,
    #[token("x")]
    StartAsciiEscape,

    #[error]
    Error,
}

#[derive(Debug, Clone, Logos)]
enum UnicodeEscape<'source> {
    // TODO: digit separators?
    #[regex(r"\{[0-9a-fA-F]*\}", |lexer| {
        let len = lexer.slice().len();
        &lexer.slice()[1..(len - 1)]
    })]
    CharCode(&'source str),
    #[regex(r"\{[^0-9a-fA-F]+\}")]
    InvalidCharCode,
    #[token("\'", |_| Quote::Single)]
    #[token("\"", |_| Quote::Double)]
    End(Quote),

    #[error]
    Error,
}

#[derive(Debug, Clone, Logos)]
enum AsciiEscape<'source> {
    #[regex(r"[0-9a-fA-F][0-9a-fA-F]?")]
    CharCode(&'source str),
    #[token("\'", |_| Quote::Single)]
    #[token("\"", |_| Quote::Double)]
    End(Quote),

    #[error]
    Error,
}

/// Literal parser state.
pub struct State<'source, 'messages> {
    range: Range,
    source: &'source str,
    message_tx: &'messages Sender<Message>,
}

impl<'source, 'messages> State<'source, 'messages> {
    pub fn new(
        range: Range,
        source: &'source str,
        message_tx: &'messages Sender<Message>,
    ) -> State<'source, 'messages> {
        State {
            range,
            source,
            message_tx,
        }
    }

    /// Report a diagnostic message.
    fn report<T>(&self, error: impl Into<Message>) -> Option<T> {
        self.message_tx.send(error.into()).unwrap();
        None
    }

    /// Get the file-relative range of the current token.
    fn token_range<Token>(&self, lexer: &logos::Lexer<'source, Token>) -> Range
    where
        Token: Logos<'source>,
    {
        let span = lexer.span();
        Range::from((self.range.start + span.start)..(self.range.start + span.end))
    }

    /// Parse a numeric literal into an unsigned integer.
    ///
    /// # Returns
    ///
    /// - `Some(_)`: If the literal was parsed correctly.
    /// - `None`: If a fatal error when parsing the literal.
    pub fn number_to_unsigned_int<T: PrimInt + Unsigned>(self) -> Option<T> {
        let mut lexer = NumericLiteral::lexer(self.source.as_bytes());

        let (base, start_digit) = match self.expect_numeric_literal_start(&mut lexer)? {
            (Sign::Positive, base, start_digit) => (base, start_digit),
            (Sign::Negative, _, _) => return self.report(NegativeUnsignedInteger(self.range)),
        };

        let mut lexer = lexer.morph();
        let mut integer = T::zero();
        let mut num_digits = 0;

        if let Some(digit) = start_digit {
            integer = self.add_integer_digit(Sign::Positive, base, integer, digit)?;
            num_digits += 1;
        }

        while let Some(token) = lexer.next() {
            let range = self.token_range(&lexer);
            match token {
                Digit36::Digit(digit) if digit < base.to_u8() => {
                    integer = self.add_integer_digit(Sign::Positive, base, integer, digit)?;
                    num_digits += 1;
                }
                Digit36::Separator if num_digits != 0 => {}
                Digit36::Separator => return self.report(ExpectedDigit(range, base)),
                Digit36::Digit(_) | Digit36::Error => match num_digits {
                    0 => return self.report(ExpectedDigit(range, base)),
                    _ => return self.report(ExpectedDigitOrSeparator(range, base)),
                },
            }
        }

        if num_digits == 0 {
            return self.report(UnexpectedEndOfLiteral(self.token_range(&lexer)));
        }

        Some(integer)
    }

    /// Parse a numeric literal into a signed integer.
    ///
    /// # Returns
    ///
    /// - `Some(_)`: If the literal was parsed correctly.
    /// - `None`: If a fatal error when parsing the literal.
    pub fn number_to_signed_int<T: PrimInt + Signed>(self) -> Option<T> {
        let mut lexer = NumericLiteral::lexer(self.source.as_bytes());

        let (sign, base, start_digit) = self.expect_numeric_literal_start(&mut lexer)?;

        let mut lexer = lexer.morph();
        let mut integer = T::zero();
        let mut num_digits = 0;

        if let Some(digit) = start_digit {
            integer = self.add_integer_digit(sign, base, integer, digit)?;
            num_digits += 1;
        }

        while let Some(token) = lexer.next() {
            let range = self.token_range(&lexer);
            match token {
                Digit36::Digit(digit) if digit < base.to_u8() => {
                    integer = self.add_integer_digit(sign, base, integer, digit)?;
                    num_digits += 1;
                }
                Digit36::Separator if num_digits != 0 => {}
                Digit36::Separator => return self.report(ExpectedDigit(range, base)),
                Digit36::Digit(_) | Digit36::Error => match num_digits {
                    0 => return self.report(ExpectedDigit(range, base)),
                    _ => return self.report(ExpectedDigitOrSeparator(range, base)),
                },
            }
        }

        if num_digits == 0 {
            return self.report(UnexpectedEndOfLiteral(self.token_range(&lexer)));
        }

        Some(integer)
    }

    /// Parse a numeric literal into a float.
    ///
    /// # Returns
    ///
    /// - `Some(_)`: If the literal was parsed correctly.
    /// - `None`: If a fatal error when parsing the literal.
    pub fn number_to_float<T: Float + From<u8>>(self) -> Option<T> {
        // NOTE: This could probably be improved a great deal.
        // It might be worth looking at `lexical-core` crate as an alternative
        // to implementing our own parser: https://github.com/Alexhuszagh/rust-lexical/

        let mut lexer = NumericLiteral::lexer(self.source.as_bytes());

        let add_digit = |sign, base: Base, float: T, digit: u8| match sign {
            Sign::Positive => float * base.to_u8().into() + digit.into(),
            Sign::Negative => float * base.to_u8().into() - digit.into(),
        };

        let (sign, base, start_digit) = self.expect_numeric_literal_start(&mut lexer)?;

        let mut float = T::zero();
        let mut num_integer_digits = 0;

        if let Some(digit) = start_digit {
            float = add_digit(sign, base, float, digit);
            num_integer_digits += 1;
        }

        if base == Base::Decimal {
            let mut lexer = lexer.morph();
            let mut has_fractional = false;
            let mut has_exponent = false;

            while let Some(token) = lexer.next() {
                let range = self.token_range(&lexer);
                match token {
                    Digit10::Digit(digit) if digit < base.to_u8() => {
                        float = add_digit(sign, base, float, digit);
                        num_integer_digits += 1;
                    }
                    Digit10::Separator if num_integer_digits != 0 => {}
                    Digit10::Separator => return self.report(ExpectedDigit(range, base)),
                    Digit10::StartFractional => {
                        has_fractional = true;
                        break;
                    }
                    Digit10::StartExponent => {
                        has_exponent = true;
                        break;
                    }
                    Digit10::Digit(_) | Digit10::Error => match num_integer_digits {
                        0 => return self.report(ExpectedDigit(range, base)),
                        _ => return self.report(ExpectedDigitSeparatorFracOrExp(range, base)),
                    },
                }
            }

            if num_integer_digits == 0 {
                return self.report(ExpectedDigit(self.token_range(&lexer), base));
            }

            if has_fractional {
                let mut frac = T::zero();
                let mut num_frac_digits = 0;

                while let Some(token) = lexer.next() {
                    let range = self.token_range(&lexer);
                    match token {
                        Digit10::Digit(digit) if digit < base.to_u8() => {
                            frac = add_digit(sign, base, frac, digit);
                            num_frac_digits += 1;
                        }
                        Digit10::Separator if num_frac_digits != 0 => {}
                        Digit10::Separator => return self.report(ExpectedDigit(range, base)),
                        Digit10::StartExponent => {
                            has_exponent = true;
                            break;
                        }
                        Digit10::Digit(_) | Digit10::StartFractional | Digit10::Error => {
                            match num_frac_digits {
                                0 => return self.report(ExpectedDigit(range, base)),
                                _ => return self.report(ExpectedDigitSeparatorOrExp(range, base)),
                            }
                        }
                    }
                }

                if num_frac_digits == 0 {
                    return self.report(ExpectedDigit(self.token_range(&lexer), base));
                }

                float = float + frac / T::powi(base.to_u8().into(), num_frac_digits);
            }

            if has_exponent {
                return self.report(FloatLiteralExponentNotSupported(self.token_range(&lexer)));
            }

            Some(float)
        } else {
            self.report(UnsupportedFloatLiteralBase(self.range, base))
        }
    }

    fn expect_numeric_literal_start(
        &self,
        lexer: &mut logos::Lexer<'source, NumericLiteral>,
    ) -> Option<(Sign, Base, Option<u8>)> {
        match self.expect_token(lexer)? {
            NumericLiteral::Sign(sign) => match self.expect_token(lexer)? {
                NumericLiteral::Base(base) => Some((sign, base, None)),
                NumericLiteral::Digit(digit) => Some((sign, Base::Decimal, Some(digit))),
                NumericLiteral::Sign(_) | NumericLiteral::Error => {
                    self.report(ExpectedRadixOrDecimalDigit(self.token_range(&lexer)))
                }
            },
            NumericLiteral::Base(base) => Some((Sign::Positive, base, None)),
            NumericLiteral::Digit(digit) => Some((Sign::Positive, Base::Decimal, Some(digit))),
            NumericLiteral::Error => {
                self.report(ExpectedStartOfNumericLiteral(self.token_range(&lexer)))
            }
        }
    }

    /// Add a new place to the given integer, handling overflow and underflow.
    fn add_integer_digit<T>(&self, sign: Sign, base: Base, integer: T, digit: u8) -> Option<T>
    where
        T: PrimInt,
    {
        T::checked_mul(&integer, &T::from(base.to_u8()).unwrap())
            .and_then(|place_shifted| match sign {
                Sign::Positive => T::checked_add(&place_shifted, &T::from(digit).unwrap()),
                Sign::Negative => T::checked_sub(&place_shifted, &T::from(digit).unwrap()),
            })
            .or_else(|| self.report(LiteralOutOfRange(self.range)))
    }

    /// Parse a quoted literal into a Unicode encoded character.
    ///
    /// # Returns
    ///
    /// - `Some(_)`: If the literal was parsed correctly.
    /// - `None`: If a fatal error when parsing the literal.
    pub fn quoted_to_unicode_char(self) -> Option<char> {
        let mut lexer = QuotedLiteral::lexer(self.source);

        let (mut lexer, end_quote) = match self.expect_token(&mut lexer)? {
            QuotedLiteral::Start(quote) => (lexer.morph(), quote),
            QuotedLiteral::Error => return self.report(InvalidToken(self.token_range(&lexer))),
        };

        let mut character = None;

        'quoted_text: loop {
            match self.expect_token(&mut lexer)? {
                QuotedText::Utf8Text(text) => {
                    for ch in text.chars() {
                        match character {
                            None => character = Some(ch),
                            Some(_) => return self.report(OverlongCharLiteral(self.range)),
                        }
                    }
                }
                QuotedText::StartEscape => match character {
                    None => match self.expect_escape_sequence(lexer.morph(), end_quote)? {
                        (_, None) => return None,
                        (escape, Some(ch)) => {
                            character = Some(ch);
                            lexer = escape.morph();
                        }
                    },
                    Some(_) => return self.report(OverlongCharLiteral(self.token_range(&lexer))),
                },
                QuotedText::End(quote) if quote == end_quote => match lexer.next() {
                    None => break 'quoted_text,
                    Some(_) => return self.report(ExpectedEndOfLiteral(self.token_range(&lexer))),
                },
                QuotedText::End(quote) => match character {
                    None => character = Some(quote.to_char()),
                    Some(_) => return self.report(OverlongCharLiteral(self.range)),
                },

                QuotedText::Error => return self.report(InvalidToken(self.token_range(&lexer))),
            }
        }

        match character {
            Some(ch) => Some(ch),
            None => self.report(EmptyCharLiteral(self.range)),
        }
    }

    /// Parse a double quoted literal into a UTF-8 encoded string.
    pub fn quoted_to_utf8_string(self) -> Option<String> {
        let mut lexer = QuotedLiteral::lexer(self.source);

        let (mut lexer, end_quote) = match self.expect_token(&mut lexer)? {
            QuotedLiteral::Start(quote) => (lexer.morph(), quote),
            QuotedLiteral::Error => return self.report(InvalidToken(self.token_range(&lexer))),
        };

        let mut string = Some(String::new());

        'quoted_text: loop {
            match self.expect_token(&mut lexer)? {
                QuotedText::Utf8Text(text) => {
                    if let Some(string) = &mut string {
                        string.push_str(text);
                    }
                }
                QuotedText::StartEscape => {
                    let (escape_lexer, ch) =
                        self.expect_escape_sequence(lexer.morph(), end_quote)?;
                    lexer = escape_lexer.morph();

                    match ch {
                        None => string = None,
                        Some(ch) => {
                            if let Some(string) = &mut string {
                                string.push(ch);
                            }
                        }
                    }
                }
                QuotedText::End(quote) if quote == end_quote => match lexer.next() {
                    None => break 'quoted_text,
                    Some(_) => return self.report(ExpectedEndOfLiteral(self.token_range(&lexer))),
                },
                QuotedText::End(quote) => {
                    if let Some(string) = &mut string {
                        string.push(quote.to_char());
                    }
                }

                QuotedText::Error => return self.report(InvalidToken(self.token_range(&lexer))),
            }
        }

        string
    }

    /// Expect another token to be present in the lexer, reporting an error if not.
    ///
    /// # Returns
    ///
    /// - `Some(_)`: If another token was found in the source stream
    /// - `None`: If we reached the end of the source stream and we need to terminate parsing
    fn expect_token<Token: Logos<'source>>(
        &self,
        lexer: &mut logos::Lexer<'source, Token>,
    ) -> Option<Token> {
        match lexer.next() {
            Some(token) => Some(token),
            None => self.report(UnexpectedEndOfLiteral(self.token_range(&lexer))),
        }
    }

    /// Expect an escape sequence.
    ///
    /// # Returns
    ///
    /// - `Some(_, Some(_))`: If we succeeded in parsing an escape sequence
    /// - `Some(_, None)`: If error ocurred but we may continue parsing in a degraded state
    /// - `None`: If a fatal error has ocurred and we need to terminate parsing the literal
    fn expect_escape_sequence(
        &self,
        mut lexer: logos::Lexer<'source, EscapeSequence>,
        end_quote: Quote,
    ) -> Option<(logos::Lexer<'source, EscapeSequence>, Option<char>)> {
        match self.expect_token(&mut lexer)? {
            EscapeSequence::Single(ch) => Some((lexer, Some(ch))),
            EscapeSequence::StartUnicodeEscape => {
                let mut unicode_lexer = lexer.morph();
                let next = self.expect_token(&mut unicode_lexer)?;
                let range = self.token_range(&unicode_lexer);
                lexer = unicode_lexer.morph();

                let ch = match next {
                    UnicodeEscape::CharCode(code) => match code.len() {
                        1..=6 => match u32::from_str_radix(code, 16).unwrap() {
                            code @ 0..=MAX_UNICODE => Some(std::char::from_u32(code).unwrap()),
                            _ => self.report(OversizedUnicodeEscapeCode(range)),
                        },
                        0 => self.report(EmptyUnicodeEscapeCode(range)),
                        _ => self.report(OverlongUnicodeEscapeCode(range)),
                    },
                    UnicodeEscape::InvalidCharCode => self.report(InvalidUnicodeEscapeCode(range)),
                    UnicodeEscape::End(quote) if end_quote == quote => {
                        return self.report(InvalidUnicodeEscape(range));
                    }
                    UnicodeEscape::End(_) | UnicodeEscape::Error => {
                        self.report(InvalidUnicodeEscape(range))
                    }
                };

                Some((lexer, ch))
            }
            EscapeSequence::StartAsciiEscape => {
                let mut ascii_lexer = lexer.morph();
                let next = self.expect_token(&mut ascii_lexer)?;
                let range = self.token_range(&ascii_lexer);
                lexer = ascii_lexer.morph();

                let ch = match next {
                    AsciiEscape::CharCode(code) if code.len() == 2 => {
                        match u32::from_str_radix(code, 16).unwrap() {
                            code @ 0..=MAX_ASCII => Some(std::char::from_u32(code).unwrap()),
                            _ => self.report(OversizedAsciiEscapeCode(range)),
                        }
                    }
                    AsciiEscape::End(quote) if end_quote == quote => {
                        return self.report(InvalidUnicodeEscape(range));
                    }
                    AsciiEscape::CharCode(_) | AsciiEscape::End(_) | AsciiEscape::Error => {
                        self.report(InvalidAsciiEscape(range))
                    }
                };

                Some((lexer, ch))
            }

            EscapeSequence::Error => {
                let range = self.token_range(&lexer);
                Some((lexer, self.report(UnknownEscapeSequence(range))))
            }
        }
    }
}
