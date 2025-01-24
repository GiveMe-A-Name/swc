use swc_common::{Span, Spanned};

use super::{
    chars::CharsConsumer,
    unicode::{is_id_continue, is_id_start, CR, FF, LF, TAB, VT, ZWJ, ZWNJ},
    LexResult, Lexer,
};
use crate::error::{Error, SyntaxError};

impl Lexer<'_> {
    /// String literals
    /// https://tc39.es/ecma262/#sec-literals-string-literals
    pub fn string_literal(&self, source: &str, span: Span) -> LexResult<String> {
        let mut chars = CharsConsumer::new(source, span);

        let mut text = String::new();

        loop {
            match chars.next() {
                None => break,
                Some('\\') => text.push(self.escape_sequence(&mut chars)?),
                Some(other) => text.push(other),
            }
        }

        Ok(text)
    }

    /// identifier
    /// https://tc39.es/ecma262/#sec-names-and-keywords
    pub fn identifier_name(&mut self, source: &str, span: Span) -> LexResult<String> {
        let mut chars = CharsConsumer::new(source, span);

        let mut text = String::new();

        let identifier_name_start = self.identifier_name_start(&mut chars)?;

        text.push(identifier_name_start);

        let identifier_name_part = self.identifier_name_part(&mut chars)?;

        text.push_str(&identifier_name_part);

        Ok(text)
    }

    /// identifier start - https://tc39.es/ecma262/#prod-IdentifierStart
    /// ```text
    /// IdentifierStart ::
    ///     IdentifierStartChar
    ///     \ UnicodeEscapeSequence
    /// ```
    fn identifier_name_start(&mut self, chars: &mut CharsConsumer) -> LexResult<char> {
        let start_loc = chars.start_loc();

        match chars.next() {
            None => {
                let end_span = chars.end_span(start_loc);
                Err(Error::new(end_span, SyntaxError::Eof))
            }
            Some(c) => match c {
                '\\' => {
                    let value = self.unicode_escape_sequence(chars)?;
                    if !is_identifier_start(value) {
                        let end_span = chars.end_span(start_loc);
                        self.emit_error_span(end_span, SyntaxError::InvalidUnicodeEscape);
                    }
                    Ok(value)
                }
                other if is_identifier_start(other) => Ok(other),
                _ => {
                    let end_span = chars.end_span(start_loc);
                    Err(Error::new(end_span, SyntaxError::InvalidUnicodeEscape))
                }
            },
        }
    }

    /// identifier part
    /// https://tc39.es/ecma262/#prod-IdentifierPart
    /// ```text
    /// IdentifierPart ::
    ///     IdentifierPartChar
    ///     \ UnicodeEscapeSequence
    /// ```
    fn identifier_name_part(&self, chars: &mut CharsConsumer) -> LexResult<String> {
        let start_loc = chars.start_loc();

        let mut text = String::new();

        while let Some(ch) = chars.peek() {
            match ch {
                '\\' => {
                    // consume '\\' char
                    chars.next();

                    let escape_char = self.escape_sequence(chars)?;
                    if !is_identifier_part(escape_char) {
                        let end_span = chars.end_span(start_loc);
                        return Err(Error::new(end_span, SyntaxError::InvalidUnicodeEscape));
                    }
                    text.push(escape_char);
                }
                other if is_identifier_part(other) => {
                    text.push(other);
                }
                _other => {
                    let end_span = chars.end_span(start_loc);

                    return Err(Error::new(end_span, SyntaxError::InvalidUnicodeEscape));
                }
            }

            chars.next();
        }

        Ok(text)
    }

    /// Escape Sequence  https://tc39.es/ecma262/#prod-EscapeSequence
    /// ```text
    /// EscapeSequence
    ///     CharacterEscapeSequence
    ///     0 [lookahead ∉ DecimalDigit]
    ///     LegacyOctalEscapeSequence
    ///     NonOctalDecimalEscapeSequence
    ///     HexEscapeSequence
    ///     UnicodeEscapeSequence
    /// ```
    pub(super) fn escape_sequence(&self, chars: &mut CharsConsumer) -> LexResult<char> {
        let start_loc = chars.start_loc();

        let ahead_char = chars.peek().ok_or_else(|| {
            let span = chars.end_span(start_loc);

            Error::new(span, SyntaxError::InvalidStrEscape)
        })?;

        let escape_char = match ahead_char {
            'b' => {
                chars.next();
                '\u{8}'
            }
            'f' => {
                chars.next();
                FF
            }
            'n' => {
                chars.next();
                LF
            }
            'r' => {
                chars.next();
                CR
            }
            't' => {
                chars.next();
                TAB
            }
            'v' => {
                chars.next();
                VT
            }
            'x' => {
                // consume 'x' char
                chars.next();

                // HexEscapeSequence ::
                //     `x` HexDigit HexDigit
                let mut value = self.hex_digit(chars).map_err(|e| {
                    Error::new(
                        e.span(),
                        SyntaxError::BadCharacterEscapeSequence {
                            expected: "2 hex characters",
                        },
                    )
                })?;
                value = (value << 4)
                    | self.hex_digit(chars).map_err(|e| {
                        Error::new(
                            e.span(),
                            SyntaxError::BadCharacterEscapeSequence {
                                expected: "2 hex characters",
                            },
                        )
                    })?;

                char::try_from(value).map_err(|_| {
                    let end_span = chars.end_span(start_loc);

                    Error::new(
                        end_span,
                        SyntaxError::BadCharacterEscapeSequence {
                            expected: "2 hex characters",
                        },
                    )
                })?
            }
            'u' => self.unicode_escape_sequence(chars)?,
            '0' => {
                // consume '0' char
                chars.next();

                // TODO: implment LegacyOctalEscapeSequence
                '\0'
            }
            other => {
                chars.next();
                other
            }
        };

        Ok(escape_char)
    }

    fn hex_digit(&self, chars: &mut CharsConsumer) -> LexResult<u32> {
        let start_loc = chars.start_loc();
        let digit = chars.next().ok_or_else(|| {
            let span = chars.end_span(start_loc);

            Error::new(span, SyntaxError::InvalidStrEscape)
        })?;

        let value = match digit {
            d @ '0'..='9' => d as u32 - '0' as u32,
            d @ 'a'..='f' => 10 + d as u32 - 'a' as u32,
            d @ 'A'..='F' => 10 + d as u32 - 'A' as u32,
            _ => {
                let span = chars.end_span(start_loc);
                return Err(Error::new(span, SyntaxError::InvalidUnicodeEscape));
            }
        };

        Ok(value)
    }

    fn unicode_escape_sequence(&self, chars: &mut CharsConsumer) -> LexResult<char> {
        let start_loc = chars.start_loc();

        match chars.next() {
            Some('u') => {}
            _ => {
                let end_span = chars.end_span(start_loc);
                return Err(Error::new(end_span, SyntaxError::InvalidUnicodeEscape));
            }
        }

        match chars.peek() {
            Some('{') => {
                let start_loc = chars.start_loc();
                chars.next();

                let value = self.code_point(chars).map_err(|e| {
                    Error::new(
                        e.span(),
                        SyntaxError::BadCharacterEscapeSequence {
                            expected: "1-6 hex characters",
                        },
                    )
                })?;

                match chars.next() {
                    Some('}') => {}
                    _ => {
                        let end_span = chars.end_span(start_loc);

                        return Err(Error::new(end_span, SyntaxError::InvalidStrEscape));
                    }
                }

                Ok(value)
            }
            _ => self.hex_4_digits(chars),
        }
    }

    fn hex_4_digits(&self, chars: &mut CharsConsumer) -> LexResult<char> {
        let mut value = 0;
        let start_loc = chars.start_loc();
        for _ in 0..4 {
            value = (value << 4)
                | self.hex_digit(chars).map_err(|e| {
                    Error::new(
                        e.span(),
                        SyntaxError::BadCharacterEscapeSequence {
                            expected: "4 hex characters",
                        },
                    )
                })?;
        }

        let end_span = chars.end_span(start_loc);
        char::try_from(value).map_err(|_| Error::new(end_span, SyntaxError::InvalidUnicodeEscape))
    }

    /// ```text
    /// 
    /// Code Point ::
    ///    HexDigits but only if MV of HexDigits ≤ 0x10FFFF
    ///
    /// HexDigits ::
    ///    HexDigit
    ///    HexDigits HexDigit
    /// ```
    fn code_point(&self, chars: &mut CharsConsumer) -> LexResult<char> {
        let mut value = self.hex_digit(chars)?;
        let start_loc = chars.start_loc();

        loop {
            let next_char = match chars.peek() {
                None => {
                    let end_span = chars.end_span(start_loc);
                    return Err(Error::new(end_span, SyntaxError::InvalidUnicodeEscape));
                }
                Some(c @ '0'..='9') => c as u32 - '0' as u32,
                Some(c @ 'a'..='f') => 10 + (c as u32 - 'a' as u32),
                Some(c @ 'A'..='F') => 10 + (c as u32 - 'A' as u32),
                Some(_) => break,
            };

            // consume next_char
            chars.next();
            value = (value << 4) | next_char;
            if value > 0x10ffff {
                let end_span = chars.end_span(start_loc);
                return Err(Error::new(end_span, SyntaxError::InvalidUnicodeEscape));
            }
        }

        char::from_u32(value).ok_or_else(|| {
            let end_span = chars.end_span(start_loc);

            Error::new(end_span, SyntaxError::InvalidUnicodeEscape)
        })
    }
}

/// True if `c` is a one-character *IdentifierStart*.
///
/// ```text
/// IdentifierStart ::
///     UnicodeIDStart
///     `$`
///     `_`
///     `\` UnicodeEscapeSequence
///
/// UnicodeIDStart ::
///     > any Unicode code point with the Unicode property "ID_Start"
/// ```
/// fork from jsparagus
fn is_identifier_start(c: char) -> bool {
    // Escaped case is handled separately.
    if c.is_ascii() {
        c == '$' || c == '_' || c.is_ascii_alphabetic()
    } else {
        is_id_start(c)
    }
}

/// True if `c` is a one-character *IdentifierPart*.
///
/// ```text
/// IdentifierPart ::
///     UnicodeIDContinue
///     `$`
///     `\` UnicodeEscapeSequence
///     <ZWNJ>
///     <ZWJ>
///
/// UnicodeIDContinue ::
///     > any Unicode code point with the Unicode property "ID_Continue"
/// ```
///
/// fork from japaragus
fn is_identifier_part(c: char) -> bool {
    // Escaped case is handled separately.
    if c.is_ascii() {
        c == '$' || c == '_' || c.is_ascii_alphanumeric()
    } else {
        is_id_continue(c) || c == ZWNJ || c == ZWJ
    }
}
