use std::str::Chars;

use swc_common::{BytePos, Span};

/// A chars consumer which tracks span.
pub(crate) struct CharsConsumer<'source> {
    inner: Chars<'source>,

    span: Span,

    offset: u32,
}

impl<'source> CharsConsumer<'source> {
    pub(crate) fn new(source: &'source str, span: Span) -> Self {
        Self {
            inner: source.chars(),
            span,
            offset: 0,
        }
    }

    /// start location of current char
    pub(crate) fn start_loc(&self) -> BytePos {
        self.span.lo + BytePos(self.offset)
    }

    /// end span of start location char
    pub(crate) fn end_span(&self, start: BytePos) -> Span {
        let end_pos = self.span.lo + BytePos(self.offset);

        Span::new(start, end_pos)
    }

    /// peek next char
    pub(crate) fn peek(&self) -> Option<char> {
        self.inner.clone().next()
    }

    pub(crate) fn consume_until(&mut self, stop_check: impl Fn(char) -> bool) -> String {
        let mut text = String::new();

        while let Some(next_char) = self.peek() {
            if stop_check(next_char) {
                break;
            }

            text.push(next_char);
            // consume next char
            self.next();
        }

        text
    }

    pub(crate) fn remainder(&self) -> &str {
        self.inner.as_str()
    }
}

impl Iterator for CharsConsumer<'_> {
    type Item = char;

    /// consume next char
    fn next(&mut self) -> Option<Self::Item> {
        self.offset += 1;

        self.inner.next()
    }
}
