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
        expr_kind => expr_kind: ExprKind;
        if_expr => if_expr: IfExpr;
        literal => literal: Literal;
        unary_expr => unary_expr: UnaryExpr;
        binary_expr => binary_expr: BinaryExpr;
        bin_op => bin_op: BinOp;
        un_op => un_op: UnOp;
        ident_expr => ident: SpannedAtom;
        return_expr => return_expr: Expr;
        let_expr => let_expr: LetExpr;
        loop_expr => loop_expr: LoopExpr;
        fn_call_expr => fn_call_expr: FnCallExpr;
        array_expr => array: Array;
        array_index_expr => array_index_expr: ArrayIndexExpr;
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
    visitor.visit_expr_kind(&expr.kind);
}

pub fn walk_expr_kind<V: Visitor>(visitor: &mut V, expr_kind: &ExprKind) {
    match expr_kind {
        ExprKind::Literal(literal) => visitor.visit_literal(literal),
        ExprKind::Ident(atom) => visitor.visit_ident_expr(atom),
        ExprKind::UnaryExpr(unary_expr) => visitor.visit_unary_expr(unary_expr),
        ExprKind::BinaryExpr(binary_expr) => visitor.visit_binary_expr(binary_expr),
        ExprKind::Return(expr) => visitor.visit_return_expr(expr),
        ExprKind::BlockExpr(expr_block) => visitor.visit_expr_block(expr_block),
        ExprKind::IfExpr(if_expr) => visitor.visit_if_expr(if_expr),
        ExprKind::LetExpr(let_expr) => visitor.visit_let_expr(let_expr),
        ExprKind::Loop(loop_expr) => visitor.visit_loop_expr(loop_expr),
        ExprKind::FnCall(fn_call_expr) => visitor.visit_fn_call_expr(fn_call_expr),
        ExprKind::Array(array) => visitor.visit_array_expr(array),
        ExprKind::ArrayIndexExpr(array_index_expr) => visitor.visit_array_index_expr(array_index_expr),
    }
}

pub fn walk_array_expr<V: Visitor>(visitor: &mut V, array_expr: &Array) {
    for expr in array_expr.elements.iter() {
        walk_expr(visitor, expr);
    }
}

pub fn walk_array_index_expr<V: Visitor>(visitor: &mut V, array_index_expr: &ArrayIndexExpr) {
    walk_expr(visitor, &array_index_expr.array);
    walk_expr(visitor, &array_index_expr.index);
}

pub fn walk_loop_expr<V: Visitor>(visitor: &mut V, loop_expr: &LoopExpr) {
    walk_expr_block(visitor, &loop_expr.body);
}

pub fn walk_fn_call_expr<V: Visitor>(visitor: &mut V, fn_call_expr: &FnCallExpr) {
    for arg in &fn_call_expr.args {
        walk_expr(visitor, arg);
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
