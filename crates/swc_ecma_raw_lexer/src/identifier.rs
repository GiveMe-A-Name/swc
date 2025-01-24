use logos::{Lexer, Logos};

use crate::RawToken;

pub fn consume_ident_part(lex: &mut Lexer<RawToken>) -> Option<()> {
    let remain = lex.remainder();
    let total_len = remain.len();

    let mut identifier_part_lexer = IdentifierPartToken::lexer(remain);

    let mut last_char_in_identifier: Option<char> = None;
    let mut consumed: usize = 0;

    while let Some(Ok(_)) = identifier_part_lexer.next() {
        last_char_in_identifier = identifier_part_lexer.slice().chars().next();
        consumed += identifier_part_lexer.slice().len();
    }

    if lex.extras.is_in_tpl && last_char_in_identifier == Some('$') {
        consumed -= '$'.len_utf8();
    }

    consumed = consumed.min(total_len);

    lex.bump(consumed);

    Some(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Logos)]
pub enum IdentifierPartToken {
    #[regex(r"([\p{XID_Continue}_$]|\\u[a-fA-F0-9]{4}|\\u\{[a-fA-F0-9]+\})")]
    IdentifierPart,
}
