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

use crate::ast::BinOp;
use crate::ast::Block;
use crate::ast::Decl;
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
        impl chumsky::Parser<$l, $i, $o, chumsky::extra::Full<Rich<$l, Token>, (), ()>> + Clone + MaybeSync
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

    pub fn parse(&self) -> Result<Vec<Decl>, Vec<Rich<'_, Token>>> {
        let tokens = lexer(&self.src);
        let result = parser(&self.ctx)
            .parse(token_stream(tokens, self.src.len()))
            .into_result();
        if self.print_error {
            print_error(&self.src, result).map_err(|_| Vec::new())
        } else {
            result
        }
    }

    pub fn parse_once_expr(&self, src: &'s str) -> Result<Expr, Vec<Rich<'_, Token>>> {
        let tokens = lexer(src);
        let result = expr_parser(&self.ctx)
            .parse(token_stream(tokens, src.len()))
            .into_result();
        if self.print_error {
            print_error(src, result).map_err(|_| Vec::new())
        } else {
            result
        }
    }

    pub fn parse_once_stmt(&self, src: &'s str) -> Result<Stmt, Vec<Rich<'_, Token>>> {
        let tokens = lexer(src);
        let result = stmt_parser(expr_parser(&self.ctx), &self.ctx)
            .parse(token_stream(tokens, src.len()))
            .into_result();
        if self.print_error {
            print_error(src, result).map_err(|_| Vec::new())
        } else {
            result
        }
    }
}

pub struct ParserContext {}

impl Default for ParserContext {
    fn default() -> Self {
        Self {}
    }
}

/* pub fn parse_expr(src: &str, print_err: bool) -> Result<Expr, Vec<Rich<Token>>> {
    let tokens = lexer(src);
    let result = expr_parser()
        .parse(token_stream(tokens, src.len()))
        .into_result();
    if print_err {
        print_error(src, result).map_err(|_| Vec::new())
    } else {
        result
    }
} */

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

fn parser<'a, Input>(ctx: &'a ParserContext) -> P!('a, Input, Vec<Decl>)
where
    Input: I<'a>,
{
    decl_parser(ctx).repeated().collect()
}

fn decl_parser<'a, Input>(ctx: &'a ParserContext) -> P!('a, Input, Decl)
where
    Input: I<'a>,
{
    let ident = select! { Token::Ident(id) => id };
    let expr = expr_parser(ctx);
    let block = block_parser(expr, ctx);

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
        .then(block)
        .map(|(((name, params), retrun_ty), body)| Decl::Fn {
            name,
            params: params.0,
            body,
            retrun_ty,
        });

    func
}

fn stmt_parser<'a, Input>(expr: P!('a, Input, Expr), _ctx: &'a ParserContext) -> P!('a, Input, Stmt)
where
    Input: I<'a>,
{
    let _let = just(Token::KeywordLet)
        .ignore_then(select! {
            Token::Ident(id) => id
        })
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .then_ignore(just(Token::Semi))
        .map(|(id, expr)| Stmt::Let(id, Box::new(expr)));

    expr.clone()
        .then_ignore(just(Token::Semi))
        .map(|expr| Stmt::SemiExpr(Box::new(expr)))
        .or(expr.map(|expr| Stmt::Expr(Box::new(expr))))
        .or(_let)
    //let expr = expr_parser();
}

fn block_parser<'a, Input>(
    expr: P!('a, Input, Expr),
    ctx: &'a ParserContext,
) -> P!('a, Input, Block)
where
    Input: I<'a>,
{
    let stmt = stmt_parser::<Input>(expr.clone(), ctx);
    stmt.clone()
        .filter(|stmt| !matches!(stmt, Stmt::Expr(_)))
        .repeated()
        .collect::<ContainerWrapper<[_; 3]>>()
        .then(stmt.filter(|stmt| matches!(stmt, Stmt::Expr(_))).or_not())
        .delimited_by(
            just(Token::OpenDelimiter(Delimiter::Brace)),
            just(Token::CloseDelimiter(Delimiter::Brace)),
        )
        .map(|(stmts, ret)| {
            let mut stmts = stmts.0;
            if let Some(ret) = ret {
                stmts.push(ret);
            }
            Block { stmts }
        })
}

fn expr_parser<'a, Input>(ctx: &'a ParserContext) -> P!('a, Input, Expr)
where
    Input: I<'a>,
{
    recursive(|_expr| {
        let expr = recursive(|expr| {
            let nested_expr = expr.clone().delimited_by(
                just(Token::OpenDelimiter(Delimiter::Parenthesis)),
                just(Token::CloseDelimiter(Delimiter::Parenthesis)),
            );

            let block = block_parser(expr.clone(), ctx);

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

            let call = (select! { Token::Ident(id) => id })
                .then(
                    expr.separated_by(just(Token::Comma))
                        .collect::<ContainerWrapper<[_; 3]>>()
                        .delimited_by(
                            just(Token::OpenDelimiter(Delimiter::Parenthesis)),
                            just(Token::CloseDelimiter(Delimiter::Parenthesis)),
                        ),
                )
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
            Token::Lt => BinOp::Lt,
            Token::Eq => BinOp::Eq,
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

        Token::Ident(id) => Expr::Ident(id),

        Token::KeywordBreak => Expr::Break,
    }
}
