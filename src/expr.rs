use crate::types::*;

use std::{ops::Deref, rc::Rc};

#[derive(Debug, Clone, Hash)]
pub enum Expr {
    Literal(LiteralExpr),
    Identifier(String),
    FunctionCall(FunctionCallExpr),
    LetBinding(LetBindingExpr),
    If(IfExpr),
    Lambda(LambdaExpr),
    FunctionDef(FunctionDefExpr),
    ValueDef(ValueDefExpr),
}

impl Expr {
    pub fn int(i: i64) -> Expr {
        Expr::Literal(LiteralExpr::Int(i))
    }

    pub fn unit() -> Expr {
        Expr::Literal(LiteralExpr::Unit)
    }

    pub fn ident(id: impl Into<String>) -> Expr {
        Expr::Identifier(id.into())
    }

    pub fn call(fun: Expr, args: &[Expr]) -> Expr {
        Expr::FunctionCall(FunctionCallExpr {
            function: ExprNode::new(fun),
            arguments: args
                .iter()
                .map(|expr| ExprNode::new(expr.clone()))
                .collect(),
        })
    }

    pub fn if_(cond: Expr, cons: Expr, alt: Expr) -> Expr {
        Expr::If(IfExpr {
            conditional: ExprNode::new(cond),
            consequent: ExprNode::new(cons),
            alternate: ExprNode::new(alt),
        })
    }

    pub fn letb(name: impl Into<String>, typ: Type, body: Expr) -> Expr {
        Expr::LetBinding(LetBindingExpr {
            binding: name.into(),
            typ,
            body: ExprNode::new(body),
        })
    }
}

#[derive(Debug, Clone, Hash)]
pub struct ExprNode {
    kind: Rc<Expr>,
    derived_type: Option<Type>,
}

impl Default for ExprNode {
    fn default() -> ExprNode {
        ExprNode {
            kind: Rc::new(Expr::Literal(LiteralExpr::Unit)),
            derived_type: None,
        }
    }
}

impl ExprNode {
    pub fn new(expr: Expr) -> ExprNode {
        ExprNode {
            kind: Rc::new(expr),
            ..Default::default()
        }
    }
}

impl AsRef<Expr> for ExprNode {
    fn as_ref(&self) -> &Expr {
        self.kind.as_ref()
    }
}

impl std::borrow::Borrow<Expr> for ExprNode {
    fn borrow(&self) -> &Expr {
        self.kind.borrow()
    }
}

impl Deref for ExprNode {
    type Target = Expr;
    fn deref(&self) -> &Expr {
        self.kind.deref()
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expr::*;
        match self {
            Literal(v) => write!(f, "{}", v),
            Identifier(id) => write!(f, "{}", id),
            FunctionCall(fc) => {
                write!(f, "({}", *fc.function)?;
                for arg in &fc.arguments[..] {
                    write!(f, " {}", **arg)?;
                }
                write!(f, ")")
            }
            If(ie) => write!(
                f,
                "(if {} {} {})",
                *ie.conditional, *ie.consequent, *ie.alternate
            ),
            _ => {
                todo!()
            }
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub enum LiteralExpr {
    Int(i64),
    // Float(f64),
    Str(String),
    // Atom(String),
    Bool(bool),
    Unit,
}

impl LiteralExpr {
    pub fn typ(&self) -> Type {
        match self {
            LiteralExpr::Int(_) => Type::Integer,
            LiteralExpr::Unit => Type::Unit,
            LiteralExpr::Str(_) => Type::String,
            LiteralExpr::Bool(_) => Type::Bool,
            // LiteralExpr::Atom(_) => Type::Atom,
        }
    }
}

impl std::fmt::Display for LiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LiteralExpr::*;
        match self {
            Int(i) => write!(f, "{}", i),
            Unit => write!(f, "'()"),
            Str(s) => write!(f, "\"{}\"", s),
            Bool(b) => write!(f, "{}", b),
            // Atom(a) => write!(f, ":{}", a),
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub struct FunctionCallExpr {
    pub function: ExprNode,
    pub arguments: Vec<ExprNode>,
}

#[derive(Debug, Clone, Hash)]
pub struct LetBindingExpr {
    pub binding: String,
    pub typ: Type,
    pub body: ExprNode,
}

#[derive(Debug, Clone, Hash)]
pub struct IfExpr {
    pub conditional: ExprNode,
    pub consequent: ExprNode,
    pub alternate: ExprNode,
}

#[derive(Debug, Clone, Hash)]
pub struct LambdaExpr {}


#[derive(Debug, Clone, Hash)]
pub struct FunctionDefExpr {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub body: ExprNode,
}

#[derive(Debug, Clone, Hash)]
pub struct ValueDefExpr {
    pub name: String,
    pub typ: Type,
    pub body: ExprNode,
}