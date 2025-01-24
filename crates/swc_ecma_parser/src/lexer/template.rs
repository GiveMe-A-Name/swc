use swc_common::Span;
use swc_ecma_raw_lexer::{RawToken, TokenState};

use super::{chars::CharsConsumer, LexResult, Lexer};
use crate::{
    error::{Error, SyntaxError},
    lexer::TokenContext,
    token::Token,
    Tokens,
};

impl Lexer<'_> {
    pub(crate) fn read_template_literal(&mut self) -> LexResult<Option<Token>> {
        assert_eq!(self.state.context.current(), Some(TokenContext::Tpl));

        self.input.set_state(TokenState {
            is_in_tpl: true,
            ..Default::default()
        });

        match self.cur()? {
            Some(RawToken::BackQuote) if self.state.last_was_tpl_element() => {
                self.input.next();

                self.input.set_state(TokenState {
                    is_in_tpl: true,
                    ..Default::default()
                });

                Ok(Some(Token::BackQuote))
            }
            Some(RawToken::DollarLBrace) if self.state.last_was_tpl_element() => {
                self.input.next();

                self.input.set_state(TokenState {
                    is_in_tpl: true,
                    ..Default::default()
                });
                Ok(Some(Token::DollarLBrace))
            }
            Some(_) => self.read_template_characters(),
            None => Ok(None),
        }
    }

    fn read_template_characters(&mut self) -> LexResult<Option<Token>> {
        let mut text = String::new();
        let start = self.start_pos();

        while let Some(cur_token) = self.cur()? {
            match cur_token {
                RawToken::BackQuote => break,
                RawToken::DollarLBrace => break,
                _ => {
                    let current_slice = self.input.cur_slice();
                    text.push_str(current_slice);
                }
            }

            self.consume_token();
        }

        let text = text.as_str();

        let end = self.end_pos();

        let span = Span::new(start, end);

        let cooked = self
            .read_template_characters_literal(text, span)
            .map(|cooked| cooked.into());

        Ok(Some(Token::Template {
            raw: text.into(),
            cooked,
        }))
    }

    fn read_template_characters_literal(&mut self, source: &str, span: Span) -> LexResult<String> {
        let mut chars = CharsConsumer::new(source, span);

        let mut text = String::new();

        loop {
            match chars.next() {
                None => break,
                Some('\\') => {
                    let start_loc = chars.start_loc();
                    text.push(self.escape_sequence(&mut chars).map_err(|e| {
                        let span = chars.end_span(start_loc);

                        Error::new(span, e.into_kind())
                    })?)
                }
                Some(other) => text.push(other),
            }
        }

        Ok(text)
    }
}
