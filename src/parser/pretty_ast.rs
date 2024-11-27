use pretty::{Doc, RcDoc};

use super::{BinOp, Expr, Literal, Program, UnOp};

pub trait ToPrettyDoc {
    fn to_doc(&self) -> RcDoc<()>;

    fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::with_capacity(1024);
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl<T> ToPrettyDoc for &T
where
    T: ToPrettyDoc,
{
    fn to_doc(&self) -> RcDoc<()> {
        T::to_doc(self)
    }
}

impl ToPrettyDoc for Program {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::intersperse(self.exprs.iter().map(|e| e.to_doc()), Doc::line())
    }
}

impl ToPrettyDoc for Expr {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Expr::Literal(literal) => literal.to_doc(),
        }
    }
}

impl ToPrettyDoc for Literal {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Literal::Integer(i) => RcDoc::as_string(i),
            Literal::Float(f) => RcDoc::as_string(f),
            Literal::String(s) => RcDoc::as_string(s),
            Literal::Boolean(b) => RcDoc::as_string(b),
            Literal::Nil => RcDoc::text("nil"),
        }
    }
}

impl ToPrettyDoc for BinOp {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::And => "&&",
            BinOp::Or => "||",
            BinOp::BitAnd => "&",
            BinOp::BitOr => "|",
            BinOp::BitXor => "^",
            BinOp::Shl => "<<",
            BinOp::Shr => ">>",
            BinOp::Eq => "==",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Ne => "!=",
            BinOp::Ge => ">=",
            BinOp::Gt => ">",
            BinOp::AddEq => "+=",
            BinOp::SubEq => "-=",
            BinOp::MulEq => "*=",
            BinOp::DivEq => "/=",
            BinOp::ModEq => "%=",
            BinOp::BitAndEq => "&=",
            BinOp::BitOrEq => "|=",
            BinOp::BitXorEq => "^=",
            BinOp::ShlEq => "<<=",
            BinOp::ShrEq => ">>=",
        })
    }
}

impl ToPrettyDoc for UnOp {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text(match self {
            UnOp::Neg => "-",
            UnOp::Not => "!",
        })
    }
}
