use pretty::{Doc, RcDoc};

use super::ast::*;

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
        RcDoc::intersperse(
            self.decls.iter().map(|d| d.to_doc()),
            RcDoc::line().append(RcDoc::line()),
        )
    }
}

impl ToPrettyDoc for Declaration {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Declaration::Func(func_decl) => func_decl.to_doc(),
        }
    }
}

impl ToPrettyDoc for FuncDecl {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("fn ")
            .append(RcDoc::as_string(&self.name.atom))
            .append("(")
            .append(RcDoc::intersperse(self.params.iter().map(RcDoc::as_string), ", ").group())
            .append(")")
            .append(self.body.to_doc())
    }
}

impl ToPrettyDoc for ExprBlock {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("{")
            .append(RcDoc::line())
            .append(RcDoc::intersperse(
                self.exprs.iter().map(|e| e.to_doc()),
                Doc::line(),
            ))
            .nest(1)
            .append("}")
    }
}

impl ToPrettyDoc for Expr {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Expr::Literal(literal) => literal.to_doc(),
            Expr::UnaryExpr(unary_expr) => unary_expr.to_doc(),
            Expr::BinaryExpr(binary_expr) => binary_expr.to_doc(),
            Expr::Ident(atom) => RcDoc::as_string(atom),
            Expr::Return(expr) => RcDoc::text("return ").append(expr.to_doc()),
            Expr::BlockExpr(expr_block) => expr_block.to_doc(),
            Expr::IfExpr(if_expr) => if_expr.to_doc(),
            Expr::LetExpr(let_expr) => todo!(),
            Expr::Loop(loop_expr) => todo!(),
            Expr::FnCall(fn_call_expr) => todo!(),
            Expr::Array(array) => todo!(),
            Expr::ArrayIndexExpr(array_index_expr) => todo!(),
        }
    }
}

impl ToPrettyDoc for IfExpr {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::text("if ")
            .append(self.cond.to_doc())
            .append(RcDoc::line())
            .append(self.then_expr.to_doc())
            .append(if let Some(el) = &self.else_expr {
                RcDoc::text(" else ").append(el.to_doc())
            } else {
                RcDoc::nil()
            })
    }
}

impl ToPrettyDoc for UnaryExpr {
    fn to_doc(&self) -> RcDoc<()> {
        self.op.to_doc().append(self.expr.to_doc())
    }
}

impl ToPrettyDoc for BinaryExpr {
    fn to_doc(&self) -> RcDoc<()> {
        RcDoc::intersperse(
            [self.left.to_doc(), self.op.to_doc(), self.right.to_doc()],
            RcDoc::space(),
        )
    }
}

impl ToPrettyDoc for Literal {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Literal::Float(n) => RcDoc::as_string(n),
            Literal::String(s) => RcDoc::as_string(s),
            Literal::Boolean(b) => RcDoc::as_string(b),
            Literal::Integer(i) => RcDoc::as_string(i),
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
            BinOp::AddAssign => "+=",
            BinOp::SubAssign => "-=",
            BinOp::MulAssign => "*=",
            BinOp::DivAssign => "/=",
            BinOp::ModAssign => "%=",
            BinOp::BitAndAssign => "&=",
            BinOp::BitOrAssign => "|=",
            BinOp::BitXorAssign => "^=",
            BinOp::ShlAssign => "<<=",
            BinOp::ShrAssign => ">>=",
            BinOp::Assign => "==",
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
