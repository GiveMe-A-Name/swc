//! This package is considered internal and should not be used by external
//! crates.
//!
//! It may updated without proper semver bump.

use std::{collections::VecDeque, fmt::Debug};

use logos::{Lexer, Logos, Skip};
use swc_common::{input::StringInput, BytePos};

use crate::{
    identifier::consume_ident_part,
    peek::{peek_nth, PeekNth},
    string::{consume_str_double_quote, consume_str_single_quote},
};

mod identifier;
mod peek;
mod regexp;
mod size_hint;
mod string;

/// A lexer that can be used to create [RawToken].
#[derive(Clone)]
pub struct RawLexer<'source> {
    lexer: PeekNth<logos::SpannedIter<'source, RawToken>>,
    lexer2: Lexer<'source, RawToken>,
    peeked: VecDeque<RawToken>,
    pos: BytePos,
    start_pos: BytePos,
}

impl Debug for RawLexer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RawBuffer {{ pos: {:?} }}", self.pos)
    }
}

#[derive(Debug, Clone, Default)]
pub struct TokenState {
    pub had_line_break: bool,
    pub is_in_tpl: bool,
}

impl<'source> RawLexer<'source> {
    pub fn new(input: StringInput<'source>) -> Self {
        let source = input.as_str();

        Self {
            lexer: peek_nth(logos::Lexer::new(source).spanned()),
            lexer2: Lexer::new(source),
            // TODO: like oxc with a number capacity
            peeked: Default::default(),
            pos: input.start_pos(),
            start_pos: input.start_pos(),
        }
    }

    fn source(&self) -> &str {
        self.lexer2.source()
    }

    pub fn start_pos(&self) -> BytePos {
        self.span().lo
    }

    pub fn end_pos(&self) -> BytePos {
        self.span().hi
    }

    pub fn cur_pos(&self) -> BytePos {
        self.span().lo
    }

    pub fn cur_hi(&mut self) -> BytePos {
        self.span().hi
    }

    pub fn update_cur_pos(&mut self) -> BytePos {
        self.span().lo
    }

    pub fn cur(&mut self) -> Result<Option<RawToken>, LogosError> {
        self.peek()
    }

    pub fn peek(&mut self) -> Result<Option<RawToken>, LogosError> {
        self.peek_nth(0)
    }

    pub fn peek_ahead(&mut self) -> Result<Option<RawToken>, LogosError> {
        self.peek_nth(1)
    }

    fn peek_nth(&mut self, n: usize) -> Result<Option<RawToken>, LogosError> {
        if let Some(peek) = self.peeked.get(n) {
            return Ok(Some(*peek));
        }

        while self.peeked.len() <= n {
            let next_token = self.lexer2.next().transpose()?;

            match next_token {
                Some(next_token) => {
                    self.peeked.push_back(next_token);
                }
                None => return Ok(None),
            }
        }

        Ok(self.peeked.get(n).cloned())
    }

    pub fn cur_char(&mut self) -> Option<char> {
        self.lexer2.remainder().chars().next()
    }

    pub fn peek_char(&mut self) -> Option<char> {
        self.lexer2.remainder().chars().nth(1)
    }

    pub fn peek_ahead_char(&mut self) -> Option<char> {
        self.lexer2.remainder().chars().nth(2)
    }

    /// Get the span for current token in input.
    pub fn span(&self) -> swc_common::Span {
        let range = self.lexer2.span();

        let start = range.start as u32;
        let end = range.end as u32;

        // +1 because hi is exclusive
        swc_common::Span {
            lo: BytePos(start + 1),
            hi: BytePos(end + 1),
        }
    }

    /// # Safety
    ///
    ///  - `start` and `end` must be within the bounds of `self.orig_str`
    pub unsafe fn slice(&self, start: BytePos, end: BytePos) -> &str {
        let lo = start.0 - self.start_pos.0;
        let hi = end.0 - self.start_pos.0;

        self.source().get_unchecked(lo as usize..hi as usize)
    }

    pub fn cur_slice(&self) -> &str {
        self.lexer2.slice()
    }

    // pub fn cur_token_value(&mut self) -> TokenValue {
    //     let cur_slice = self.cur_slice();
    //     let cur_token = self.cur();
    // }

    /// # Safety
    ///
    /// - `n` must be equal or smaller than  lefting length of `self.orig_str`
    pub fn bump(&mut self, n: usize) {
        self.lexer2.bump(n);
        // self.reset_peeked();

        // self.lexer.inner_mut().bump(n);
        // if let Some((_, span)) = self.lexer.peek() {
        //     self.pos = self.base_pos + BytePos(span.start as u32);
        // }
    }

    // pub fn eat(&mut self, token: RawToken) -> Result<bool, LogosError> {
    //     let Ok(Some(cur)) = self.cur() else {
    //         return false;
    //     };

    //     if cur == token {
    //         self.next();
    //         true
    //     } else {
    //         false
    //     }
    // }

    pub fn is_ascii(&mut self, c: u8) -> bool {
        self.cur_char() == Some(c as char)
    }

    pub fn eat_ascii(&mut self, c: u8) -> bool {
        let cur = self.cur_char();

        if cur == Some(c as char) {
            unsafe {
                // Safety: We already checked for the current char
                self.bump(1);
            }
            true
        } else {
            false
        }
    }

    ///# Safety
    ///
    /// - `pos` must be within the bounds of `self.orig_str`
    pub unsafe fn reset_to(&mut self, pos: BytePos) {
        self.lexer2 = Lexer::new(self.lexer2.source());
    }

    pub fn state_mut(&mut self) -> &mut TokenState {
        &mut self.lexer2.extras
    }

    pub fn state(&self) -> &TokenState {
        &self.lexer2.extras
    }

    pub fn set_state(&mut self, state: TokenState) {
        self.lexer2.extras = state;
    }

    // fn reset_peeked(&mut self) {
    //     unsafe {
    //         // let lo = self.pos.0 - self.start_pos.0;

    //         // Safety: We already checked for the current char
    //         // let source = self.orig_str.get_unchecked(lo as usize..);

    //         // self.lexer = peek_nth(logos::Lexer::new(source).spanned());
    //     }
    // }
}

impl Iterator for RawLexer<'_> {
    type Item = Result<RawToken, LogosError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_token) = self.peeked.pop_front() {
            return Some(Ok(next_token));
        }

        let next_token = self.lexer2.next()?;

        let item = match next_token {
            Ok(item) => item,
            Err(e) => return Some(Err(e)),
        };

        Some(Ok(item))
    }
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(error = LogosError, extras = TokenState)]
pub enum RawToken {
    #[token("=>")]
    Arrow,

    #[token("#")]
    Hash,

    #[token("@")]
    At,

    #[token(".")]
    Dot,

    #[token("...")]
    DotDotDot,

    #[token("!")]
    Bang,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("{")]
    /// Token of '{'
    LBrace,

    #[token("}")]
    /// Token of '}'
    RBrace,

    #[token(";")]
    Semi,

    #[token(",")]
    Comma,

    #[token(":")]
    Colon,

    #[token("`")]
    BackQuote,

    #[token("${")]
    DollarLBrace,

    #[token("?")]
    QuestionMark,

    #[token("++")]
    PlusPlus,

    #[token("--")]
    MinusMinus,

    #[token("~")]
    Tilde,

    #[token("'", callback = consume_str_single_quote)]
    #[token("\"", callback = consume_str_double_quote)]
    Str,

    #[regex(r"(?:0|[1-9]\d*)(\.\d*)?(?:[eE][+-]?\d+)?")]
    #[regex(r"0[xX][a-fA-F0-9]+")]
    #[regex(r"0[oO][0-7]+")]
    #[regex(r"0[bB][01]+")]
    Num,

    #[regex(r#"0[0-7]+"#)]
    LegacyOctalNum,

    #[regex(r#"[0-9]+n"#)]
    BigInt,

    #[token("null")]
    Null,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("==")]
    EqEqOp,

    #[token("!=")]
    NotEqOp,

    #[token("===")]
    EqEqEqOp,

    #[token("!==")]
    NotEqEqOp,

    #[token("<")]
    /// Token of '<'
    LtAngle,

    #[token("<=")]
    LtEqOp,

    #[token(">")]
    /// Token of '>'
    GtAngle,

    #[token(">=")]
    GtEqOp,

    #[token("<<")]
    LShiftOp,

    #[token(">>")]
    RShiftOp,

    #[token(">>>")]
    ZeroFillRShiftOp,

    #[token("+")]
    AddOp,

    #[token("-")]
    SubOp,

    #[token("*")]
    MulOp,

    #[token("/")]
    DivOp,

    #[token("%")]
    ModOp,

    #[token("|")]
    BitOrOp,

    #[token("^")]
    BitXorOp,

    #[token("&")]
    BitAndOp,

    #[token("**")]
    ExpOp,

    #[token("||")]
    LogicalOrOp,

    #[token("&&")]
    LogicalAndOp,

    #[token("??")]
    NullishCoalescingOp,

    #[token("=")]
    AssignOp,

    #[token("+=")]
    AddAssignOp,

    #[token("-=")]
    SubAssignOp,

    #[token("*=")]
    MulAssignOp,

    #[token("/=")]
    DivAssignOp,

    #[token("%=")]
    ModAssignOp,

    #[token("<<=")]
    LShiftAssignOp,

    #[token(">>=")]
    RShiftAssignOp,

    #[token(">>>=")]
    ZeroFillRShiftAssignOp,

    #[token("|=")]
    BitOrAssignOp,

    #[token("^=")]
    BitXorAssignOp,

    #[token("&=")]
    BitAndAssignOp,

    #[token("**=")]
    ExpAssignOp,

    #[token("&&=")]
    AndAssignOp,

    #[token("||=")]
    OrAssignOp,

    #[token("??=")]
    NullishAssignOp,

    /// TODO: Move jsx lexing to RawLexer
    JsxTagStart,
    /// TODO: Move jsx lexing to RawLexer
    JsxTagEnd,

    #[regex(
        r"([\p{XID_Start}_$]|\\u[a-fA-F0-9]{4}|\\u\{[a-fA-F0-9]+\})",
        priority = 10,
        callback = consume_ident_part,
    )]
    Ident,

    /// Line terminator https://tc39.es/ecma262/#sec-line-terminators
    ///
    /// ```text
    /// LineTerminator ::
    ///     <LF> (U+000A)
    ///     <CR> (U+000D)
    ///     <LS> (U+2028)
    ///     <PS> (U+2029)
    ///
    /// LineTerminatorSequence ::
    ///     <LF>
    ///     <CR> [lookahead ≠ <LF>]
    ///     <LS>
    ///     <PS>
    ///     <CR> <LF>
    /// ```
    #[regex(r"(\u{000d}\u{000a}|[\u{000a}\u{000d}\u{2029}\u{2029}])", priority = 5, callback = newline_callback)]
    NewLine,

    #[regex(r"[ \t]+")]
    Whitespace,

    #[regex(r"//[^\n]*")]
    LineComment,

    #[regex(r"/\*(?:[^*]|\*[^/])*\*/")]
    BlockComment,

    #[token("<!--")]
    LegacyCommentOpen,

    #[token("-->")]
    LegacyCommentClose,

    #[token("<<<<<<<")]
    #[token(">>>>>>>")]
    #[token("=======")]
    #[token("|||||||")]
    ConflictMarker,

    #[token("await")]
    Await,

    #[token("break")]
    Break,

    #[token("case")]
    Case,

    #[token("catch")]
    Catch,

    #[token("continue")]
    Continue,

    #[token("debugger")]
    Debugger,

    #[token("default")]
    Default_,

    #[token("do")]
    Do,

    #[token("else")]
    Else,

    #[token("finally")]
    Finally,

    #[token("for")]
    For,

    #[token("function")]
    Function,

    #[token("if")]
    If,

    #[token("return")]
    Return,

    #[token("switch")]
    Switch,

    #[token("throw")]
    Throw,

    #[token("try")]
    Try,

    #[token("var")]
    Var,

    #[token("let")]
    Let,

    #[token("const")]
    Const,

    #[token("while")]
    While,

    #[token("with")]
    With,

    #[token("new")]
    New,

    #[token("this")]
    This,

    #[token("super")]
    Super,

    #[token("class")]
    Class,

    #[token("extends")]
    Extends,

    #[token("export")]
    Export,

    #[token("import")]
    Import,

    #[token("yield")]
    Yield,

    #[token("in")]
    In,

    #[token("instanceof")]
    InstanceOf,

    #[token("typeof")]
    TypeOf,

    #[token("void")]
    Void,

    #[token("delete")]
    Delete,

    #[token("abstract")]
    Abstract,

    #[token("as")]
    As,

    #[token("async")]
    Async,

    #[token("from")]
    From,

    #[token("of")]
    Of,

    #[token("type")]
    Type,

    #[token("global")]
    Global,

    #[token("static")]
    Static,

    #[token("using")]
    Using,

    #[token("readonly")]
    Readonly,

    #[token("unique")]
    Unique,

    #[token("keyof")]
    Keyof,

    #[token("declare")]
    Declare,

    #[token("enum")]
    Enum,

    #[token("is")]
    Is,

    #[token("infer")]
    Infer,

    Symbol,

    #[token("undefined")]
    Undefined,

    #[token("interface")]
    Interface,

    #[token("implements")]
    Implements,

    #[token("asserts")]
    Asserts,

    #[token("require")]
    Require,

    #[token("get")]
    Get,

    #[token("set")]
    Set,

    #[token("any")]
    Any,

    #[token("intrinsic")]
    Intrinsic,

    #[token("unknown")]
    Unknown,

    /// Ts types - string
    #[token("string")]
    String,

    #[token("object")]
    Object,

    /// Ts types - number
    #[token("number")]
    Number,

    #[token("bigint")]
    Bigint,

    #[token("boolean")]
    Boolean,

    #[token("never")]
    Never,

    #[token("assert")]
    Assert,

    #[token("namespace")]
    Namespace,

    #[token("accessor")]
    Accessor,

    #[token("meta")]
    Meta,

    #[token("target")]
    Target,

    #[token("satisfies")]
    Satisfies,

    #[token("package")]
    Package,

    #[token("protected")]
    Protected,

    #[token("private")]
    Private,

    #[token("public")]
    Public,

    #[token(r"\")]
    BackSlash,
}

fn newline_callback(l: &mut Lexer<RawToken>) -> RawToken {
    l.extras.had_line_break = true;

    RawToken::NewLine
}

impl RawToken {
    pub fn is_line_terminator(&self) -> bool {
        matches!(self, RawToken::NewLine)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum LogosError {
    #[default]
    UnknownChar,
    UnterminatedStr,
}
