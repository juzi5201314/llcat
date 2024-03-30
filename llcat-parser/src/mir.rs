mod basic_block;
mod statement;
mod module;
mod translator;

use petgraph::data::Build;
use petgraph::Graph;

use crate::ast::Decl;
use crate::small_vec::SmallVec3;

use self::basic_block::BasicBlock;
use self::statement::Statement;

pub struct Mir {
    graph: Graph<BasicBlock, ()>,
}

impl Mir {
    pub fn new() -> Self {
        Self {
            graph: Graph::new(),
        }
    }

    pub fn from_ast(ast: &Vec<Decl>) -> Self {
        let mut mir = Mir::new();
        let graph = &mut mir.graph;

        for decl in ast {
            match decl {
                Decl::Fn {
                    name,
                    params,
                    body,
                    retrun_ty,
                } => {
                    let mut local = 1;
                    let mut basic_block = BasicBlock::from_label(name.clone());
                    for _param in params {
                        basic_block
                            .stmts
                            .push(Statement::new(statement::StatementKind::Local(Local::new(
                                local,
                            ))));
                        local += 1;
                    }

                    for stmt in &body.stmts {
                        
                    }

                    graph.add_node(basic_block);
                }
            }
        }

        mir
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Local {
    id: usize,
}

impl Local {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}
