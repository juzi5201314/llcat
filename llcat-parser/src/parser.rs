use ariadne::Color;
use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;
use ariadne::Source;
use chumsky::error::Rich;

use chumsky::input::ValueInput;
use chumsky::primitive::just;
use chumsky::recursive::recursive;
use chumsky::select;
use chumsky::span::SimpleSpan;
use chumsky::util::MaybeSync;
use chumsky::IterParser;
use chumsky::Parser as _;
use smol_str::SmolStr;

use crate::ast::BinOp;
use crate::ast::Block;
use crate::ast::Decl;
use crate::ast::Import;
use crate::ast::Item;
use crate::ast::Module;
use crate::ast::Path;
use crate::ast::Stmt;
use crate::ast::UnOp;
use crate::ast::{Expr, Literal};
use crate::small_vec::ContainerWrapper;
use crate::token::lexer;
use crate::token::token_stream;
use crate::token::Delimiter;
use crate::token::Token;

macro_rules! P {
    ($l:lifetime, $i:ty, $o:ty) => {
        impl chumsky::Parser<$l, $i, $o, chumsky::extra::Full<Rich<$l, Token>, ParserContext, ()>> + Clone + MaybeSync
    };
}

pub trait I<'a>: ValueInput<'a, Token = Token, Span = SimpleSpan> {}
impl<'a, T> I<'a> for T where T: ValueInput<'a, Token = Token, Span = SimpleSpan> {}

pub struct Parser<'s> {
    pub src: &'s str,
    pub ctx: ParserContext,
    print_error: bool,
}

impl<'s> Parser<'s> {
    pub fn new(src: &'s str) -> Self {
        Self {
            src,
            ctx: ParserContext::default(),
            print_error: true,
        }
    }

    pub fn with_ctx(src: &'s str, ctx: ParserContext) -> Self {
        Self {
            src,
            ctx,
            print_error: true,
        }
    }

    pub fn without_print_error(mut self) -> Self {
        self.print_error = false;
        self
    }

    pub fn no_check(mut self) -> Self {
        self.ctx.no_check = true;
        self
    }

    pub fn parse(&mut self) -> Result<Module, Vec<Rich<'_, Token>>> {
        let tokens = lexer(&self.src);
        let result = parser()
            .parse_with_state(token_stream(tokens, self.src.len()), &mut self.ctx)
            .into_result();
        if self.print_error {
            print_error(&self.src, result).map_err(|_| Vec::new())
        } else {
            result
        }
    }

    pub fn parse_once_expr(&mut self, src: &'s str) -> Result<Expr, Vec<Rich<'_, Token>>> {
        let tokens = lexer(src);
        let result = expr_parser()
            .parse_with_state(token_stream(tokens, src.len()), &mut self.ctx)
            .into_result();
        if self.print_error {
            print_error(src, result).map_err(|_| Vec::new())
        } else {
            result
        }
    }

    pub fn parse_once_stmt(&mut self, src: &'s str) -> Result<Stmt, Vec<Rich<'_, Token>>> {
        let tokens = lexer(src);
        let result = stmt_parser(expr_parser())
            .parse_with_state(token_stream(tokens, src.len()), &mut self.ctx)
            .into_result();
        if self.print_error {
            print_error(src, result).map_err(|_| Vec::new())
        } else {
            result
        }
    }
}

pub struct ParserContext {
    scope: Vec<small_map::SmallMap<16, SmolStr, (), ahash::RandomState>>,

    no_check: bool,
}

impl Default for ParserContext {
    fn default() -> Self {
        let mut root_scope = Vec::with_capacity(8);
        root_scope.push(small_map::SmallMap::new());
        Self {
            scope: root_scope,
            no_check: false,
        }
    }
}

impl ParserContext {
    fn enter(&mut self) {
        self.scope.push(small_map::SmallMap::new());
    }

    fn exit(&mut self) {
        self.scope.pop();
    }

    fn local(&mut self, id: SmolStr) {
        self.scope.last_mut().unwrap().insert(id, ());
    }

    fn has(&self, key: &str) -> bool {
        if self.no_check {
            return true;
        }
        self.scope
            .iter()
            .find(|scope| scope.get(key).is_some())
            .is_some()
    }
}

pub fn print_error<T>(src: &str, result: Result<T, Vec<Rich<Token>>>) -> Result<T, ()> {
    match result {
        Ok(t) => Ok(t),
        Err(errs) => {
            for err in errs {
                let err = err.map_token(|c| c.to_string());
                Report::build(ReportKind::Error, (), err.span().start)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new(err.span().into_range())
                            .with_message(err.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint(Source::from(src))
                    .unwrap();
            }
            Err(())
        }
    }
}

fn parser<'a, Input>() -> P!('a, Input, Module)
where
    Input: I<'a>,
{
    top_item_parser()
        .repeated()
        .collect::<Vec<_>>()
        .map(|items| Module { items })
}

fn top_item_parser<'a, Input>() -> P!('a, Input, Item)
where
    Input: I<'a>,
{
    import_parser()
        .map(Item::Import)
        .or(decl_parser().map(Item::Decl))
}

fn import_parser<'a, Input>() -> P!('a, Input, Import)
where
    Input: I<'a>,
{
    let ident = ident_parser::<Input>();
    let import = just(Token::KeywordImport)
        .ignore_then(
            ident
                .clone()
                .separated_by(just(Token::PathSep))
                .collect::<ContainerWrapper<[_; 3]>>(),
        )
        .map_with(|path, _| Import::Direct(Path { path: path.0 }));

    import
}

fn decl_parser<'a, Input>() -> P!('a, Input, Decl)
where
    Input: I<'a>,
{
    let ident = ident_parser();
    let expr = expr_parser();
    let block = block_parser(expr);

    let func = just(Token::KeywordFn)
        .ignore_then(ident.clone())
        .then(
            ident
                .clone()
                .separated_by(just(Token::Comma))
                .collect::<ContainerWrapper<[_; 6]>>()
                .delimited_by(
                    just(Token::OpenDelimiter(Delimiter::Parenthesis)),
                    just(Token::CloseDelimiter(Delimiter::Parenthesis)),
                ),
        )
        .then(ident.or_not())
        .map_with(|o, e| {
            e.state().enter();
            // add self to scope (recursion)
            e.state().local(o.0 .0.clone());
            let params = &o.0 .1;
            for param in &params.0 {
                e.state().local(param.clone());
            }
            o
        })
        .then(block)
        .map_with(|(((name, params), retrun_ty), mut body), e| {
            // exit func block scope
            e.state().exit();
            // this func
            e.state().local(name.clone());

            match body.stmts.pop() {
                Some(Stmt::Expr(e)) if !matches!(&*e, Expr::Return(_)) => {
                    body.stmts.push(Stmt::Expr(Box::new(Expr::Return(e))))
                }
                Some(stmt) => body.stmts.push(stmt),
                None => {}
            }

            Decl::Fn {
                name,
                params: params.0,
                body,
                retrun_ty,
            }
        });

    func
}

fn stmt_parser<'a, Input>(expr: P!('a, Input, Expr)) -> P!('a, Input, Stmt)
where
    Input: I<'a>,
{
    let _let = just(Token::KeywordLet)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .then_ignore(just(Token::Semi))
        .map_with(|(id, expr), e| {
            e.state().local(id.clone());
            Stmt::Let(id, Box::new(expr))
        });

    let assign = ident_parser()
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .then_ignore(just(Token::Semi).or_not())
        .validate(|(id, expr), e, emitter| {
            if !e.state().has(&id) {
                emitter.emit(Rich::custom(
                    e.span(),
                    format!("cannot find id `{}` in this scope", &id),
                ))
            }
            Stmt::Assign(id, Box::new(expr))
        });

    _let.or(assign)
        .or(expr.then(just(Token::Semi).or_not()).map(|(expr, semi)| {
            if semi.is_some() {
                Stmt::SemiExpr(Box::new(expr))
            } else {
                Stmt::Expr(Box::new(expr))
            }
        }))
}

fn block_parser<'a, Input>(expr: P!('a, Input, Expr)) -> P!('a, Input, Block)
where
    Input: I<'a>,
{
    let stmt = stmt_parser::<Input>(expr.clone());
    stmt.clone()
        .map_with(|o, e| {
            e.state().enter();
            o
        })
        .filter(|stmt| {
            !matches!(stmt, Stmt::Expr(_))
                || matches!(
                    stmt,
                    Stmt::Expr(
                        box Expr::Block(_)
                        | box Expr::If(_, _, _)
                        | box Expr::Loop(_)
                        | box Expr::Break,
                    )
                )
        })
        .repeated()
        .collect::<ContainerWrapper<[_; 3]>>()
        .then(stmt.filter(|stmt| matches!(stmt, Stmt::Expr(_))).or_not())
        .delimited_by(
            just(Token::OpenDelimiter(Delimiter::Brace)),
            just(Token::CloseDelimiter(Delimiter::Brace)),
        )
        .map_with(|(stmts, ret), e| {
            e.state().exit();
            let mut stmts = stmts.0;
            if let Some(ret) = ret {
                stmts.push(ret);
            }
            Block { stmts }
        })
}

fn expr_parser<'a, Input>() -> P!('a, Input, Expr)
where
    Input: I<'a>,
{
    recursive(|_expr| {
        let expr = recursive(|expr| {
            let nested_expr = expr.clone().delimited_by(
                just(Token::OpenDelimiter(Delimiter::Parenthesis)),
                just(Token::CloseDelimiter(Delimiter::Parenthesis)),
            );

            let block = block_parser(expr.clone());

            let atom = atom_parser();
            let binops = binop_parser();

            let _if = just(Token::KeywordIf)
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(just(Token::KeywordElse).ignore_then(block.clone()).or_not())
                .map(|((cond, block), else_block)| Expr::If(Box::new(cond), block, else_block));

            let ret = just(Token::KeywordRet)
                .ignore_then(expr.clone())
                //.then_ignore(just(Token::Semi).or_not())
                .map(|expr| Expr::Return(Box::new(expr)));

            let _loop = just(Token::KeywordLoop)
                .ignore_then(block.clone())
                .map(|block| Expr::Loop(block));

            let call = value_parser()
                .then(
                    expr.separated_by(just(Token::Comma))
                        .collect::<ContainerWrapper<[_; 3]>>()
                        .delimited_by(
                            just(Token::OpenDelimiter(Delimiter::Parenthesis)),
                            just(Token::CloseDelimiter(Delimiter::Parenthesis)),
                        ),
                )
                .validate(|o, e, emitter| {
                    let s: &mut ParserContext = e.state();
                    if !s.has(&o.0) {
                        emitter.emit(Rich::custom(
                            e.span(),
                            format!("cannot find id `{}` in this scope", &o.0),
                        ))
                    }
                    o
                })
                .map(|(id, args)| Expr::Call(id, Box::new(args.0)));

            let p1 = call
                .or(atom)
                .or(block.map(|block| Expr::Block(block)))
                .or(nested_expr)
                .or(_if)
                .or(_loop)
                .or(ret);

            let unary = unop_parser()
                .repeated()
                .foldr(p1, |unop, expr| Expr::Unary(unop, Box::new(expr)));

            let p2 = unary;

            let mut binary = p2.boxed();

            for op in binops {
                binary = binary
                    .clone()
                    .foldl(op.then(binary.clone()).repeated(), |l, (op, r)| {
                        Expr::Binary(op, Box::new(l), Box::new(r))
                    })
                    .boxed();
            }

            binary
        });

        expr
    })
}

fn unop_parser<'a, Input>() -> P!('a, Input, UnOp)
where
    Input: I<'a>,
{
    select! {
        Token::Minus => UnOp::Neg,
        Token::Not => UnOp::Not,
    }
}

fn binop_parser<'a, Input>() -> [P!('a, Input, BinOp); 10]
where
    Input: I<'a>,
{
    [
        // as
        select! {
            Token::Star => BinOp::Mul,
            Token::Slash => BinOp::Div,
            Token::Percent => BinOp::Mod,
        }
        .boxed(),
        select! {
            Token::Plus => BinOp::Add,
            Token::Minus => BinOp::Sub,
        }
        .boxed(),
        select! {
            Token::Shl => BinOp::Shl,
            Token::Shr => BinOp::Shr,
        }
        .boxed(),
        select! {
            Token::And => BinOp::BitAnd,
        }
        .boxed(),
        select! {
            Token::Caret => BinOp::BitXor,
        }
        .boxed(),
        select! {
            Token::Or => BinOp::BitOr,
        }
        .boxed(),
        select! {
            Token::Lt => BinOp::Lt,
            Token::Gt => BinOp::Gt,
            Token::Le => BinOp::Le,
            Token::Ge => BinOp::Ge,
            Token::Ne => BinOp::Ne,
        }
        .boxed(),
        select! {
            Token::AndAnd => BinOp::And,
        }
        .boxed(),
        select! {
            Token::OrOr => BinOp::Or,
        }
        .boxed(),
        // .. | ..=
        select! {
            Token::EqEq => BinOp::Eq,
            Token::PlusEq => BinOp::AddEq,
            Token::MinusEq => BinOp::SubEq,
            Token::StarEq => BinOp::MulEq,
            Token::SlashEq => BinOp::DivEq,
            Token::PercentEq => BinOp::ModEq,
            Token::AndEq => BinOp::BitAndEq,
            Token::CaretEq => BinOp::BitXorEq,
            Token::OrEq => BinOp::BitOrEq,
            Token::ShlEq => BinOp::ShlEq,
            Token::ShrEq => BinOp::ShrEq,
        }
        .boxed(),
    ]
}

fn atom_parser<'a, Input>() -> P!('a, Input, Expr)
where
    Input: I<'a>,
{
    select! {
        Token::Interger(i) => Expr::Literal(Literal::Interger(i)),
        Token::Float(f) => Expr::Literal(Literal::Float(f)),
        Token::Boolean(b) => Expr::Literal(Literal::Boolean(b)),
        Token::String(s) => Expr::Literal(Literal::String(s)),

        Token::KeywordBreak => Expr::Break,
    }
    .or(value_parser().map(Expr::Ident))
}

fn ident_parser<'a, Input>() -> P!('a, Input, SmolStr)
where
    Input: I<'a>,
{
    select! {
        Token::Ident(id) => id,
    }
}

fn value_parser<'a, Input>() -> P!('a, Input, SmolStr)
where
    Input: I<'a>,
{
    select! {
        Token::Ident(id) => id,
    }
    .labelled("identifier")
    .validate(|id, e, emitter| {
        let s: &mut ParserContext = e.state();
        if !s.has(&id) {
            emitter.emit(Rich::custom(
                e.span(),
                format!("cannot find local `{}` in this scope", &id),
            ))
        }
        id
    })
}
