pub type SurfProg<Ann> = Exp<Ann>;
pub type SurfFunDecl<Ann> = FunDecl<Exp<Ann>, Ann>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunDecl<E, Ann> {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: E,
    pub ann: Ann,
}

/* Expressions */
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Exp<Ann> {
    Num(i64, Ann),
    Bool(bool, Ann),
    Char(char, Ann),
    Var(String, Ann),
    Prim(Prim, Vec<Box<Exp<Ann>>>, Ann),
    Let {
        bindings: Vec<(String, Exp<Ann>)>,
        body: Box<Exp<Ann>>,
        ann: Ann,
    },
    If {
        cond: Box<Exp<Ann>>,
        thn: Box<Exp<Ann>>,
        els: Box<Exp<Ann>>,
        ann: Ann,
    },
    Semicolon {
        e1: Box<Exp<Ann>>,
        e2: Box<Exp<Ann>>,
        ann: Ann,
    },
    FunDefs {
        decls: Vec<FunDecl<Exp<Ann>, Ann>>,
        body: Box<Exp<Ann>>,
        ann: Ann,
    },

    // Source program calls will be parsed as a call.
    // In your lambda_lift function you should
    Call(String, Vec<Exp<Ann>>, Ann),

    // An internal tail call to a locally defined function.
    InternalTailCall(String, Vec<Exp<Ann>>, Ann),
    // A call to one of the top-level function definitions
    // Uses the Snake Calling Convention v0
    // marked to indicate whether it is a tail call or not
    ExternalCall {
        fun_name: String,
        args: Vec<Exp<Ann>>,
        is_tail: bool,
        ann: Ann,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Prim {
    // unary
    Add1,
    Sub1,
    Not,
    Print,
    IsBool,
    IsNum,
    //1arg
    IsArray,
    //1 arg
    IsChar,
    //1 arg
    IsString,
    //two arguments
    Range,
    //0 or more arguments
    MakeString,
    //2 arguments
    HeapSet,
    //1 argument
    HeapGet,
    //0 or more arguments
    MakeArray,

    Length,
    


    // binary
    Add,
    Sub,
    Mul,
    And,
    Or,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Neq,
}

/* Sequential Expressions */
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SeqProg<Ann> {
    pub funs: Vec<FunDecl<SeqExp<Ann>, Ann>>,
    pub main: SeqExp<Ann>,
    pub ann: Ann,
}

pub type SeqFunDecl<Ann> = FunDecl<SeqExp<Ann>, Ann>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ImmExp {
    Num(i64),
    Bool(bool),
    Char(char),
    Var(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SeqExp<Ann> {
    Imm(ImmExp, Ann),
    Prim(Prim, Vec<ImmExp>, Ann),
    Let {
        var: String,
        bound_exp: Box<SeqExp<Ann>>,
        body: Box<SeqExp<Ann>>,
        ann: Ann,
    },
    // Local function definitions
    // These should only be called using InternalTailCall
    FunDefs {
        decls: Vec<FunDecl<SeqExp<Ann>, Ann>>,
        body: Box<SeqExp<Ann>>,
        ann: Ann,
    },
    If {
        cond: ImmExp,
        thn: Box<SeqExp<Ann>>,
        els: Box<SeqExp<Ann>>,
        ann: Ann,
    },
    // An internal tail call to a locally defined function.
    // Implemented by setting arguments and then jmp in Assembly
    InternalTailCall(String, Vec<ImmExp>, Ann),
    // A call to one of the top-level function definitions
    // Uses the Snake Calling Convention v0
    // marked to indicate whether it is a tail call or not
    ExternalCall {
        fun_name: String,
        args: Vec<ImmExp>,
        is_tail: bool,
        ann: Ann,
    },
    Semicolon {
        e1: Box<SeqExp<Ann>>,
        e2: Box<SeqExp<Ann>>,
        ann: Ann,
    },
}

/* Useful functions for Exps, SeqExps */
impl<Ann> Exp<Ann> {
    pub fn ann(&self) -> Ann
    where
        Ann: Clone,
    {
        match self {
            Exp::Num(_, a)
            | Exp::Bool(_, a)
            | Exp::Var(_, a)
            | Exp::Char(_, a)
            | Exp::Prim(_, _, a)
            | Exp::Let { ann: a, .. }
            | Exp::If { ann: a, .. }
            | Exp::Call(_, _, a)
            | Exp::Semicolon { e1:_, e2:_, ann: a }
            | Exp::FunDefs { ann: a, .. }
            | Exp::InternalTailCall(_, _, a)
            | Exp::ExternalCall { ann: a, .. } => a.clone(),
        }
    }

    pub fn ann_mut(&mut self) -> &mut Ann {
        match self {
            Exp::Num(_, a)
            | Exp::Bool(_, a)
            | Exp::Var(_, a)
            | Exp::Char(_, a)
            | Exp::Prim(_, _, a)
            | Exp::Let { ann: a, .. }
            | Exp::If { ann: a, .. }
            | Exp::Call(_, _, a)
            | Exp::Semicolon { e1:_, e2:_, ann: a }
            | Exp::FunDefs { ann: a, .. }
            | Exp::InternalTailCall(_, _, a)
            | Exp::ExternalCall { ann: a, .. } => a,
        }
    }

    pub fn map_ann<Ann2, F>(&self, f: &mut F) -> Exp<Ann2>
    where
        F: FnMut(&Ann) -> Ann2,
    {
        match self {
            Exp::Num(n, a) => Exp::Num(*n, f(a)),
            Exp::Bool(b, a) => Exp::Bool(*b, f(a)),
            Exp::Char(c, a) => Exp::Char(*c, f(a)),
            Exp::Var(s, a) => Exp::Var(s.clone(), f(a)),
            Exp::Prim(op, es, a) => Exp::Prim(
                *op,
                es.iter().map(|e| Box::new(e.map_ann(f))).collect(),
                f(a),
            ),
            Exp::Let {
                bindings,
                body,
                ann,
            } => Exp::Let {
                bindings: bindings
                    .iter()
                    .map(|(x, e)| (x.clone(), e.map_ann(f)))
                    .collect(),
                body: Box::new(body.map_ann(f)),
                ann: f(ann),
            },
            Exp::If {
                cond,
                thn,
                els,
                ann,
            } => Exp::If {
                cond: Box::new(cond.map_ann(f)),
                thn: Box::new(thn.map_ann(f)),
                els: Box::new(els.map_ann(f)),
                ann: f(ann),
            },
            Exp::Call(fun, args, ann) => Exp::Call(
                fun.clone(),
                args.iter().map(|e| e.map_ann(f)).collect(),
                f(ann),
            ),
            Exp::InternalTailCall(fun, args, ann) => Exp::InternalTailCall(
                fun.clone(),
                args.iter().map(|e| e.map_ann(f)).collect(),
                f(ann),
            ),
            Exp::ExternalCall {
                fun_name,
                args,
                is_tail,
                ann,
            } => Exp::ExternalCall {
                fun_name: fun_name.clone(),
                args: args.iter().map(|e| e.map_ann(f)).collect(),
                is_tail: *is_tail,
                ann: f(ann),
            },
            Exp::FunDefs { decls, body, ann } => Exp::FunDefs {
                decls: decls.iter().map(|d| d.map_ann(f)).collect(),
                body: Box::new(body.map_ann(f)),
                ann: f(ann),
            },
            Exp::Semicolon { e1, e2, ann } => Exp::Semicolon { 
                e1: Box::new(e1.map_ann(f)),
                e2: Box::new(e2.map_ann(f)),
                ann: f(ann) 
            },
        }
    }
}

impl<Ann> SeqExp<Ann> {
    pub fn ann(&self) -> Ann
    where
        Ann: Clone,
    {
        match self {
            SeqExp::Imm(_, a)
            | SeqExp::Prim(_, _, a)
            | SeqExp::Let { ann: a, .. }
            | SeqExp::If { ann: a, .. }
            | SeqExp::InternalTailCall(_, _, a)
            | SeqExp::ExternalCall { ann: a, .. }
            | SeqExp::Semicolon { e1: _, e2: _, ann: a }
            | SeqExp::FunDefs { ann: a, .. } => a.clone(),
        }
    }

    pub fn map_ann<Ann2, F>(&self, f: &mut F) -> SeqExp<Ann2>
    where
        F: FnMut(&Ann) -> Ann2,
    {
        match self {
            SeqExp::Imm(imm, a) => SeqExp::Imm(imm.clone(), f(a)),
            SeqExp::Prim(op, imms, a) => SeqExp::Prim(*op, imms.to_vec(), f(a)),
            SeqExp::Let {
                var,
                bound_exp,
                body,
                ann,
            } => SeqExp::Let {
                var: var.clone(),
                bound_exp: Box::new(bound_exp.map_ann(f)),
                body: Box::new(body.map_ann(f)),
                ann: f(ann),
            },
            SeqExp::If {
                cond,
                thn,
                els,
                ann,
            } => SeqExp::If {
                cond: cond.clone(),
                thn: Box::new(thn.map_ann(f)),
                els: Box::new(els.map_ann(f)),
                ann: f(ann),
            },
            SeqExp::InternalTailCall(fun, args, ann) => {
                SeqExp::InternalTailCall(fun.clone(), args.clone(), f(ann))
            }
            SeqExp::ExternalCall {
                fun_name,
                args,
                is_tail,
                ann,
            } => SeqExp::ExternalCall {
                fun_name: fun_name.clone(),
                args: args.clone(),
                is_tail: *is_tail,
                ann: f(ann),
            },
            SeqExp::FunDefs { decls, body, ann } => SeqExp::FunDefs {
                decls: decls.iter().map(|d| d.map_ann(f)).collect(),
                body: Box::new(body.map_ann(f)),
                ann: f(ann),
            },
            SeqExp::Semicolon { e1, e2, ann } => SeqExp::Semicolon { 
                e1: Box::new(e1.map_ann(f)), 
                e2: Box::new(e2.map_ann(f)), 
                ann: f(ann) 
            }
        }
    }
}

impl<Ann> FunDecl<Exp<Ann>, Ann> {
    pub fn map_ann<Ann2, F>(&self, f: &mut F) -> FunDecl<Exp<Ann2>, Ann2>
    where
        F: FnMut(&Ann) -> Ann2,
    {
        FunDecl {
            name: self.name.clone(),
            parameters: self.parameters.clone(),
            body: self.body.map_ann(f),
            ann: f(&self.ann),
        }
    }
}

impl<Ann> SeqProg<Ann> {
    pub fn map_ann<Ann2, F>(&self, f: &mut F) -> SeqProg<Ann2>
    where
        F: FnMut(&Ann) -> Ann2,
    {
        SeqProg {
            funs: self.funs.iter().map(|d| d.map_ann(f)).collect(),
            main: self.main.map_ann(f),
            ann: f(&self.ann),
        }
    }
}
impl<Ann> FunDecl<SeqExp<Ann>, Ann> {
    pub fn map_ann<Ann2, F>(&self, f: &mut F) -> FunDecl<SeqExp<Ann2>, Ann2>
    where
        F: FnMut(&Ann) -> Ann2,
    {
        FunDecl {
            name: self.name.clone(),
            parameters: self.parameters.clone(),
            body: self.body.map_ann(f),
            ann: f(&self.ann),
        }
    }
}

use std::fmt;
fn print_indent(indent: usize) -> String {
    "    ".repeat(indent)
}

impl<Ann> FunDecl<Exp<Ann>, Ann> {
    fn prettify(&self, indent: usize) -> String {
        format!(
            "def {}({}):\n{}{}",
            self.name,
            self.parameters.join(", "),
            print_indent(indent + 1),
            self.body.prettify(indent + 1)
        )
    }
}

impl<Ann> fmt::Display for FunDecl<Exp<Ann>, Ann> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.prettify(0))
    }
}

impl<Ann> Exp<Ann> {
    fn prettify(&self, indent: usize) -> String {
        let id = print_indent(indent);
        match self {
            Exp::Bool(b, _) => {
                if *b {
                    String::from("true")
                } else {
                    String::from("false")
                }
            }
            Exp::Call(callee, args, _) => {
                format!(
                    "{}({})",
                    callee,
                    args.into_iter()
                        .map(|arg| arg.prettify(indent))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Exp::Var(x, _) => format!("{}", x),
            Exp::Char(c, _) => format!("{}", c),
            Exp::Num(x, _) => format!("{}", x),
            Exp::Prim(prim, args, _) => match prim {
                Prim::Add1 => format!("add1({})", args[0].prettify(indent)),
                Prim::Sub1 => format!("sub1({})", args[0].prettify(indent)),
                Prim::Not => format!("!({})", args[0].prettify(indent)),
                Prim::Print => format!("print({})", args[0].prettify(indent)),
                Prim::IsBool => format!("isBool({})", args[0].prettify(indent)),
                Prim::IsNum => format!("isNum({})", args[0].prettify(indent)),
                Prim::IsChar => format!("isChar({})", args[0].prettify(indent)),
                Prim::IsArray => format!("isArray({})", args[0].prettify(indent)),
                Prim::IsString => format!("IsString({})", args[0].prettify(indent)),
                Prim::Length => {"Length".to_string()},
                Prim::Add => format!(
                    "({} + {})",
                    args[0].prettify(indent),
                    args[1].prettify(indent)
                ),
                Prim::Sub => format!(
                    "({} - {})",
                    args[0].prettify(indent),
                    args[1].prettify(indent)
                ),
                Prim::Mul => format!(
                    "({} * {})",
                    args[0].prettify(indent),
                    args[1].prettify(indent)
                ),
                Prim::And => format!(
                    "({} && {})",
                    args[0].prettify(indent),
                    args[1].prettify(indent)
                ),
                Prim::Or => format!(
                    "({} || {})",
                    args[0].prettify(indent),
                    args[1].prettify(indent)
                ),
                Prim::Lt => format!(
                    "({} < {})",
                    args[0].prettify(indent),
                    args[1].prettify(indent)
                ),
                Prim::Gt => format!(
                    "({} > {})",
                    args[0].prettify(indent),
                    args[1].prettify(indent)
                ),
                Prim::Le => format!(
                    "({} <= {})",
                    args[0].prettify(indent),
                    args[1].prettify(indent)
                ),
                Prim::Ge => format!(
                    "({} >= {})",
                    args[0].prettify(indent),
                    args[1].prettify(indent)
                ),
                Prim::Eq => format!(
                    "({} == {})",
                    args[0].prettify(indent),
                    args[1].prettify(indent)
                ),
                Prim::Neq => format!(
                    "({} != {})",
                    args[0].prettify(indent),
                    args[1].prettify(indent)
                ),
                Prim::MakeArray => {
                    let a: Vec<String> = args.iter().map(|f| f.prettify(indent)).collect();
                    let a_print = a.join(", ");
                    format!("[{}]", a_print)
                },
                Prim::MakeString => {
                    let a: Vec<String> = args.iter().map(|f| f.prettify(indent)).collect();
                    let a_print = a.join("");
                    format!("{}", a_print)
                }
                Prim::HeapGet => {
                    format!("{}[{}]",
                    args[0].prettify(indent),
                    args[1].prettify(indent))
                },
                Prim::HeapSet => {
                    format!("{}[{}] := {}", 
                    args[0].prettify(indent), 
                    args[1].prettify(indent), 
                    args[2].prettify(indent))
                },
                Prim::Range => {
                    format!("{}[{}..{}]",
                    args[0].prettify(indent),
                    args[1].prettify(indent),
                    args[2].prettify(indent))
                }
            },
            Exp::Let { bindings, body, .. } => {
                format!(
                    "let {} in\n{}{}",
                    bindings
                        .into_iter()
                        .map(|(name, bd)| format!("{} = {}", name, bd.prettify(indent)))
                        .collect::<Vec<String>>()
                        .join(", "),
                    id,
                    body.prettify(indent)
                )
            }
            Exp::If { cond, thn, els, .. } => {
                format!(
                    "if {}:\n{}{}\n{}else:\n{}{}",
                    cond.prettify(indent),
                    print_indent(indent + 1),
                    thn.prettify(indent + 1),
                    id,
                    print_indent(indent + 1),
                    els.prettify(indent + 1)
                )
            }
            Exp::FunDefs { decls, body, .. } => {
                format!(
                    "\n{}{}\n{}in\n{}{}",
                    id,
                    decls
                        .into_iter()
                        .map(|decl| decl.prettify(indent))
                        .collect::<Vec<String>>()
                        .join(format!("\n{}and\n", id).as_str()),
                    id,
                    id,
                    body.prettify(indent)
                )
            }
            Exp::ExternalCall { fun_name, args, .. } => {
                format!(
                    "ECall({}, {})",
                    fun_name,
                    args.into_iter()
                        .map(|arg| arg.prettify(indent))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Exp::InternalTailCall(fun_name, args, ..) => {
                format!(
                    "ICall({}, {})",
                    fun_name,
                    args.into_iter()
                        .map(|arg| arg.prettify(indent))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Exp::Semicolon { e1, e2, ann:_ } => {
                format!(
                    "{};
{}",
                    e1.prettify(indent),
                    e2.prettify(indent)
                )
            }
        }
    }
}

impl<Ann> fmt::Display for Exp<Ann> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.prettify(0))
    }
}
