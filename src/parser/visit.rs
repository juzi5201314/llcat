#![allow(unused_variables)]

use super::{ast::*, Atom};

macro_rules! visit_fns {
    ($($node_name:ident => $($arg:ident : $typ:ty)+);* $(;)?) => {
        $(
            paste::paste!(
                #[inline]
                fn [<visit_ $node_name>](&mut self, $($arg: &$typ)+) {
                    [<walk_ $node_name>](self, $($arg)+);
                }
            );
        )*
    };
}

pub trait Visitor: Sized {
    visit_fns!(
        program => program: Program;
        declaration => declaration: Declaration;
        func_decl => decl: FuncDecl;
        expr_block => block: ExprBlock;
        expr => expr: Expr;
        if_expr => if_expr: IfExpr;
        literal => literal: Literal;
        unary_expr => unary_expr: UnaryExpr;
        binary_expr => binary_expr: BinaryExpr;
        bin_op => bin_op: BinOp;
        un_op => un_op: UnOp;
        ident_expr => ident: Atom;
        return_expr => return_expr: Expr;
        let_expr => let_expr: LetExpr;
    );
}

pub fn walk_program<V: Visitor>(visitor: &mut V, program: &Program) {
    for decl in &program.decls {
        visitor.visit_declaration(decl);
    }
}

pub fn walk_declaration<V: Visitor>(visitor: &mut V, declaration: &Declaration) {
    match declaration {
        Declaration::Func(func_decl) => visitor.visit_func_decl(func_decl),
    }
}

pub fn walk_func_decl<V: Visitor>(visitor: &mut V, decl: &FuncDecl) {
    visitor.visit_expr(&decl.body);
}

pub fn walk_expr_block<V: Visitor>(visitor: &mut V, block: &ExprBlock) {
    for expr in &block.exprs {
        visitor.visit_expr(expr);
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    match expr {
        Expr::Literal(literal) => visitor.visit_literal(literal),
        Expr::Ident(atom) => visitor.visit_ident_expr(atom),
        Expr::UnaryExpr(unary_expr) => visitor.visit_unary_expr(unary_expr),
        Expr::BinaryExpr(binary_expr) => visitor.visit_binary_expr(binary_expr),
        Expr::Return(expr) => visitor.visit_return_expr(expr),
        Expr::BlockExpr(expr_block) => visitor.visit_expr_block(expr_block),
        Expr::IfExpr(if_expr) => visitor.visit_if_expr(if_expr),
        Expr::LetExpr(let_expr) => visitor.visit_let_expr(let_expr),
    }
}

pub fn walk_if_expr<V: Visitor>(visitor: &mut V, if_expr: &IfExpr) {
    walk_expr(visitor, &if_expr.cond);
    walk_expr(visitor, &if_expr.then_expr);
    if let Some(el) = &if_expr.else_expr {
        walk_expr(visitor, el);
    }
}

pub fn walk_return_expr<V: Visitor>(visitor: &mut V, expr: &Expr) {
    walk_expr(visitor, expr);
}

pub fn walk_let_expr<V: Visitor>(visitor: &mut V, let_expr: &LetExpr) {
    walk_expr(visitor, &let_expr.value);
}

pub fn walk_literal<V: Visitor>(visitor: &mut V, literal: &Literal) {}

pub fn walk_ident_expr<V: Visitor>(visitor: &mut V, ident: &Atom) {}

pub fn walk_binary_expr<V: Visitor>(visitor: &mut V, binary_expr: &BinaryExpr) {
    visitor.visit_expr(&binary_expr.left);
    visitor.visit_bin_op(&binary_expr.op);
    visitor.visit_expr(&binary_expr.right);
}

pub fn walk_unary_expr<V: Visitor>(visitor: &mut V, unary_expr: &UnaryExpr) {
    visitor.visit_un_op(&unary_expr.op);
    visitor.visit_expr(&unary_expr.expr);
}

pub fn walk_bin_op<V: Visitor>(visitor: &mut V, bin_op: &BinOp) {}

pub fn walk_un_op<V: Visitor>(visitor: &mut V, un_op: &UnOp) {}
