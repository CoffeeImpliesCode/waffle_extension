use crate::expr;
use anyhow::{anyhow, Result};
use logos::Logos;
use std::error::Error;
use std::ops::Range;

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+")]
pub enum ExprToken {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("'")]
    Quote,
    #[token(",")]
    Comma,
    #[token("=>")]
    BigArrow,
    #[token("->")]
    Arrow,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("=")]
    #[token("==")]
    Eq,
    #[token("def")]
    Def,
    #[token("fn")]
    Fn,
    #[token("if")]
    If,
    #[token("let")]
    Let,
    #[token("#t")]
    True,
    #[token("#f")]
    False,
    #[regex(r#"[+-]?[0-9]+"#)]
    Int,
    #[regex(r#"[#a-zA-Z_][#a-zA-Z0-9_]*"#)]
    Symbol,
    #[regex(r#"'[a-zA-Z0-9_]+"#)]
    Atom,
    #[regex(r#""[^"]*""#)]
    Str,
}

impl std::fmt::Display for ExprToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprToken::LParen => write!(f, "("),
            ExprToken::RParen => write!(f, ")"),
            ExprToken::Quote => write!(f, "'"),
            ExprToken::Comma => write!(f, ","),
            ExprToken::BigArrow => write!(f, "=>"),
            ExprToken::Arrow => write!(f, "->"),
            ExprToken::Plus => write!(f, "+"),
            ExprToken::Minus => write!(f, "-"),
            ExprToken::Star => write!(f, "*"),
            ExprToken::Slash => write!(f, "/"),
            ExprToken::Eq => write!(f, "="),
            ExprToken::Def => write!(f, "def"),
            ExprToken::Fn => write!(f, "fn"),
            ExprToken::If => write!(f, "if"),
            ExprToken::Let => write!(f, "let"),
            ExprToken::True => write!(f, "#t"),
            ExprToken::False => write!(f, "#f"),
            ExprToken::Int => write!(f, "int"),
            ExprToken::Symbol => write!(f, "symbol"),
            ExprToken::Atom => write!(f, "atom"),
            ExprToken::Str => write!(f, "str"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Cursor<'a> {
    src: &'a str,
    tokens: &'a [ExprToken],
    ranges: &'a [Range<usize>],
    pos: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(src: &'a str, tokens: &'a [ExprToken], ranges: &'a [Range<usize>]) -> Cursor<'a> {
        Cursor {
            src,
            tokens,
            ranges,
            pos: 0,
        }
    }

    pub fn current(&self) -> Result<ExprToken> {
        self.tokens
            .get(self.pos)
            .ok_or(anyhow!("Current token ({}) does not exist.", self.pos))
            .cloned()
    }

    pub fn idx(&self) -> usize {
        self.pos
    }

    pub fn remaining(&self) -> &[ExprToken] {
        &self.tokens[self.pos..self.tokens.len()]
    }

    pub fn consume(&mut self, n: usize) {
        self.pos += n;
    }

    pub fn range(&self, idx: usize) -> Result<Range<usize>> {
        self.ranges
            .get(idx)
            .ok_or(anyhow!("Current range does not exist."))
            .cloned()
    }

    pub fn peek(&self) -> Result<ExprToken> {
        self.tokens
            .get(self.pos + 1)
            .ok_or(anyhow!("Peek: end of input"))
            .cloned()
    }

    pub fn next(&mut self) -> Result<ExprToken> {
        self.pos += 1;
        self.current()
    }

    pub fn prev(&mut self) -> Result<ExprToken> {
        match self.pos.checked_sub(1) {
            Some(pos) => self.pos = pos,
            None => return Err(anyhow!("Previous token does not exist")),
        }
        self.current()
    }

    pub fn peek_back(&self) -> Result<ExprToken> {
        let pos = match self.pos.checked_sub(1) {
            Some(pos) => pos,
            None => return Err(anyhow!("Peek: Previous token does not exist")),
        };
        self.tokens
            .get(pos)
            .ok_or(anyhow!("Peek: no token at pos {pos}"))
            .cloned()
    }

    pub fn peekn(&self, n: isize) -> Result<ExprToken> {
        let pos = if n < 0 {
            match self.pos.checked_sub(n.abs() as usize) {
                Some(pos) => pos,
                None => return Err(anyhow!("Peek: Token {n} does not exist.")),
            }
        } else {
            self.pos + n as usize
        };
        self.tokens
            .get(pos)
            .ok_or(anyhow!("Peek: Token {n} does not exist."))
            .cloned()
    }

    pub fn expect(&mut self, tok: ExprToken) -> Result<()> {
        if let Ok(t) = self.current() {
            if t == tok {
                self.consume(1);
                Ok(())
            } else {
                self.consume(1);
                Err(anyhow!("Expected `{tok}`, got `{t}`"))
            }
        } else {
            Err(anyhow!("Expected token `{tok:?}`, got end of input."))
        }
    }

    pub fn expect_any(&mut self, toks: &[ExprToken]) -> Result<()> {
        if let Ok(t) = self.next() {
            if toks.contains(&t) {
                Ok(())
            } else {
                Err(anyhow!("Expected any of {:?}, got {:?}.", toks, t))
            }
        } else {
            Err(anyhow!("Expected any of `{toks:?}`, got end of input."))
        }
    }

    pub fn source(&self) -> Result<&str> {
        if let Ok(idx) = self.range(self.idx()) {
            Ok(&self.src[idx])
        } else {
            Err(anyhow!("No source for current token."))
        }
    }

    pub fn sourcen(&self, n: isize) -> Result<&str> {
        if let Some(idx) = self.idx().checked_add_signed(n) {
            if let Ok(idx) = self.range(idx) {
                Ok(&self.src[idx])
            } else {
                Err(anyhow!("No source for token at index range {idx}."))
            }
        } else {
            Err(anyhow!(""))
        }
    }
}

pub fn tokenize(input: &str) -> Result<(Vec<ExprToken>, Vec<Range<usize>>)> {
    let mut tokens = vec![];
    let mut ranges = vec![];
    let mut lexer = ExprToken::lexer(input).spanned();

    while let Some(token) = lexer.next() {
        match token.0 {
            Ok(tok) => {
                tokens.push(tok);
                ranges.push(token.1);
            }
            Err(e) => {
                return Err(anyhow!(
                    "Unrecognized token `{}` at [{}..{}]",
                    &input[token.1.clone()],
                    token.1.start,
                    token.1.end
                ))
            }
        }
    }
    Ok((tokens, ranges))
}

pub fn parse(input: &str) -> Result<expr::Expr> {
    let (tokens, ranges) = tokenize(input)?;
    let mut cursor = Cursor::new(input, &tokens, &ranges);
    parse_expr(&mut cursor)
}

pub fn parse_expr(c: &mut Cursor) -> Result<expr::Expr> {
    use ExprToken::*;
    match c.remaining() {
        [] => Err(anyhow!("Unexpected end of input.")),
        [Int, ..] => {
            let val = c.source()?.parse::<i64>()?;
            c.consume(1);
            Ok(expr::Expr::Literal(expr::LiteralExpr::Int(val)))
        }
        [Quote, LParen, RParen, ..] => {
            c.consume(3);
            Ok(expr::Expr::Literal(expr::LiteralExpr::Unit))
        }
        [Plus, ..] => {
            c.consume(1);
            Ok(expr::Expr::Identifier("+".into()))
        }
        [Minus, ..] => {
            c.consume(1);
            Ok(expr::Expr::Identifier("-".into()))
        }
        [Star, ..] => {
            c.consume(1);
            Ok(expr::Expr::Identifier("*".into()))
        }
        [Slash, ..] => {
            c.consume(1);
            Ok(expr::Expr::Identifier("/".into()))
        }
        [Eq, ..] => {
            c.consume(1);
            Ok(expr::Expr::Identifier("==".into()))
        }
        [LParen, Def, ..] => {
            todo!()
        }
        [LParen, Let, ..] => {
            todo!()
        }
        [LParen, If, ..] => {
            c.consume(2);
            let conditional = expr::ExprNode::new(parse_expr(c)?);
            let consequent = expr::ExprNode::new(parse_expr(c)?);
            let alternate = expr::ExprNode::new(parse_expr(c)?);
            c.expect(RParen)?;
            Ok(expr::Expr::If(expr::IfExpr {
                conditional,
                consequent,
                alternate,
            }))
        }
        [LParen, Fn, ..] => {
            todo!()
        }
        [LParen, ..] => {
            c.consume(1);
            let function = expr::ExprNode::new(parse_expr(c)?);
            let mut arguments = vec![];
            while c.current()? != RParen {
                arguments.push(expr::ExprNode::new(parse_expr(c)?));
            }
            c.expect(RParen)?;
            Ok(expr::Expr::FunctionCall(expr::FunctionCallExpr {
                function,
                arguments,
            }))
        }
        [Str, ..] => {
            let val = c.source()?.split_at(1).1;
            let val = val.split_at(val.len() - 1).0;
            let val = val.to_string();
            c.consume(1);
            Ok(expr::Expr::Literal(expr::LiteralExpr::Str(val)))
        }
        [Symbol, ..] => {
            let val = c.source()?.to_string();
            c.consume(1);
            Ok(expr::Expr::Identifier(val))
        }
        [True, ..] => {
            c.consume(1);
            Ok(expr::Expr::Literal(expr::LiteralExpr::Bool(true)))
        }
        [False, ..] => {
            c.consume(1);
            Ok(expr::Expr::Literal(expr::LiteralExpr::Bool(false)))
        }
        // [Atom, ..] => {
        //     let val = c.source()?.split_at(1).1.to_string();
        //     c.consume(1);
        //     Ok(expr::Expr::Literal(expr::LiteralExpr::Atom(val)))
        // }
        a => Err(anyhow!("Unexpected remaining sequence of tokens {:?}", a)),
    }
}
