// semantic.rs
use crate::parser::ast::*;
use crate::parser::visit::*;
use crate::parser::Atom;
use hashbrown::{HashMap, HashSet};

#[derive(Debug)]
pub enum SemanticError {
    UndefinedVariable(Atom),
    Redeclaration(Atom),
    ArityMismatch {
        name: Atom,
        expected: usize,
        found: usize,
    },
    UndefinedFunction(Atom),
    // Add other error types as needed
}

pub struct SemanticAnalyzer {
    scopes: Vec<Scope>,
    functions: HashMap<Atom, FunctionInfo>,
    errors: Vec<SemanticError>,
    current_function: Option<Atom>,
}

struct Scope {
    variables: HashSet<Atom>,
}

struct FunctionInfo {
    params: Vec<Atom>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            functions: HashMap::new(),
            errors: Vec::new(),
            current_function: None,
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Vec<SemanticError> {
        // First pass: collect function declarations
        for decl in &program.decls {
            if let Declaration::Func(func_decl) = decl {
                self.functions.insert(
                    func_decl.name.clone(),
                    FunctionInfo {
                        params: func_decl.params.clone(),
                    },
                );
            }
        }

        // Second pass: analyze declarations
        for decl in &program.decls {
            self.visit_declaration(decl);
        }

        std::mem::take(&mut self.errors)
    }

    fn enter_scope(&mut self) {
        self.scopes.push(Scope {
            variables: HashSet::new(),
        });
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_scope(&mut self, f: impl FnOnce(&mut Scope)) {
        self.current_scope_mut().map(f);
    }

    fn current_scope_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.last_mut()
    }

    fn lookup_variable(&self, name: &Atom) -> bool {
        self.scopes
            .iter()
            .rev()
            .any(|scope| scope.variables.contains(name))
    }
}

impl Visitor for SemanticAnalyzer {
    fn visit_declaration(&mut self, decl: &Declaration) {
        match decl {
            Declaration::Func(func_decl) => {
                self.current_function = Some(func_decl.name.clone());
                self.enter_scope();

                // Add parameters to scope
                self.current_scope(|scope| {
                    for param in &func_decl.params {
                        scope.variables.insert(param.clone());
                    }
                });

                self.visit_expr(&func_decl.body);
                self.exit_scope();
                self.current_function = None;
            } // Handle other declaration types if needed
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::LetExpr(let_expr) => {
                walk_expr(self, &let_expr.value);
            }

            Expr::FnCall(fn_call) => {
                match self.functions.get(&fn_call.name) {
                    Some(info) => {
                        if fn_call.args.len() != info.params.len() {
                            self.errors.push(SemanticError::ArityMismatch {
                                name: fn_call.name.clone(),
                                expected: info.params.len(),
                                found: fn_call.args.len(),
                            });
                        }
                    }
                    None => {
                        self.errors
                            .push(SemanticError::UndefinedFunction(fn_call.name.clone()));
                    }
                }

                for arg in &fn_call.args {
                    self.walk_expr(arg);
                }
            }

            Expr::Ident(ident) => {
                if !self.lookup_variable(ident) {
                    self.errors
                        .push(SemanticError::UndefinedVariable(ident.clone()));
                }
            }

            Expr::Block(block) => {
                self.enter_scope();
                self.walk_expr_block(block);
                self.exit_scope();
            }

            // Handle other expression types
            _ => walk_expr(self, expr),
        }
    }
}
