use crate::asm::instrs_to_string;
use crate::asm::{Arg32, Arg64, BinArgs, Instr, MemRef, MovArgs, Reg, Reg32};
use crate::syntax::{Exp, FunDecl, ImmExp, Prim, SeqExp, SeqProg, SurfFunDecl, SurfProg};

use crate::list::List;

use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;

#[derive(Debug, PartialEq, Eq)]
pub enum CompileErr<Span> {
    UnboundVariable {
        unbound: String,
        location: Span,
    },
    UndefinedFunction {
        undefined: String,
        location: Span,
    },
    // The Span here is the Span of the let-expression that has the two duplicated bindings
    DuplicateBinding {
        duplicated_name: String,
        location: Span,
    },

    Overflow {
        num: i64,
        location: Span,
    },

    DuplicateFunName {
        duplicated_name: String,
        location: Span, // the location of the 2nd function
    },

    DuplicateArgName {
        duplicated_name: String,
        location: Span,
    },

    FunctionUsedAsValue {
        function_name: String,
        location: Span,
    },

    ValueUsedAsFunction {
        variable_name: String,
        location: Span,
    },

    FunctionCalledWrongArity {
        function_name: String,
        correct_arity: usize,
        arity_used: usize,
        location: Span, // location of the function *call*
    },
}

static MAX_INT: i64 = i64::MAX >> 1;
static MIN_INT: i64 = i64::MIN >> 1;
static SCRATCH: Reg = Reg::R8;

#[derive(Debug, Clone)]
struct SurfEnv<'e, FVal, VVal> {
    funs: Vec<(&'e str, FVal)>,
    vars: Vec<(&'e str, VVal)>,
}

pub fn get<T>(env: &[(&str, T)], x: &str) -> Option<T>
where
    T: Clone,
{
    for (y, n) in env.iter().rev() {
        if x == *y {
            return Some(n.clone());
        }
    }
    None
}

impl<'e, FVal, VVal> SurfEnv<'e, FVal, VVal> {
    fn new() -> Self {
        SurfEnv {
            funs: Vec::new(),
            vars: Vec::new(),
        }
    }
    fn get_fun(&self, f: &'e str) -> Option<FVal>
    where
        FVal: Clone,
    {
        get(&self.funs, f)
    }
    fn push_fun(&mut self, x: &'e str, v: FVal) {
        self.funs.push((x, v))
    }

    fn get_var(&self, x: &'e str) -> Option<VVal>
    where
        VVal: Clone,
    {
        get(&self.vars, x)
    }
    fn push_var(&mut self, x: &'e str, v: VVal) {
        self.vars.push((x, v))
    }

    fn all_vars(&self) -> Vec<&'e str> {
        self.vars.iter().map(|(x, _)| *x).collect()
    }

    fn all_funs(&self) -> Vec<&'e str> {
        self.funs.iter().map(|(x, _)| *x).collect()
    }
}

fn check_exp<'exp, Span>(
    e: &'exp Exp<Span>,
    mut env: SurfEnv<'exp, usize, ()>,
) -> Result<(), CompileErr<Span>>
where
    Span: Clone,
{
    fn check_decl<'e, Span>(
        d: &'e SurfFunDecl<Span>,
        mut env: SurfEnv<'e, usize, ()>,
    ) -> Result<(), CompileErr<Span>>
    where
        Span: Clone,
    {
        let mut names: HashSet<&str> = HashSet::new();
        for p_name in d.parameters.iter() {
            if names.contains(p_name.as_str()) {
                return Err(CompileErr::DuplicateArgName {
                    duplicated_name: p_name.to_string(),
                    location: d.ann.clone(),
                });
            }
            names.insert(p_name);
            env.push_var(p_name, ());
        }
        check_exp(&d.body, env)
    }

    match e {
        Exp::Bool(_, _) => Ok(()),
        Exp::Num(n, span) => {
            if *n > MAX_INT || *n < MIN_INT {
                Err(CompileErr::Overflow {
                    num: *n,
                    location: span.clone(),
                })
            } else {
                Ok(())
            }
        },
        Exp::Char(_, _) => Ok(()),
        Exp::Var(x, span) => match env.get_var(x) {
            None => match env.get_fun(x) {
                None => Err(CompileErr::UnboundVariable {
                    unbound: x.clone(),
                    location: span.clone(),
                }),
                Some(_) => Err(CompileErr::FunctionUsedAsValue {
                    function_name: x.clone(),
                    location: span.clone(),
                }),
            },
            Some(_) => Ok(()),
        },
        Exp::Prim(_, es, _) => {
            for e in es {
                check_exp(e, env.clone())?;
            }
            Ok(())
        }
        Exp::Let {
            bindings,
            body,
            ann: span,
        } => {
            let mut names: HashSet<&str> = HashSet::new();
            for (x, e) in bindings {
                if names.contains(x.as_str()) {
                    return Err(CompileErr::DuplicateBinding {
                        duplicated_name: String::from(x),
                        location: span.clone(),
                    });
                }
                check_exp(e, env.clone())?;
                names.insert(x);
                env.push_var(x, ())
            }
            check_exp(body, env)?;
            Ok(())
        }
        Exp::If { cond, thn, els, .. } => {
            check_exp(cond, env.clone())?;
            check_exp(thn, env.clone())?;
            check_exp(els, env.clone())
        }
        Exp::Call(f, args, span) => match env.get_fun(f) {
            None => Err(CompileErr::UndefinedFunction {
                undefined: f.clone(),
                location: span.clone(),
            }),
            Some(arity) => {
                if arity != args.len() {
                    return Err(CompileErr::FunctionCalledWrongArity {
                        function_name: f.clone(),
                        correct_arity: arity,
                        arity_used: args.len(),
                        location: span.clone(),
                    });
                }
                for arg in args {
                    check_exp(arg, env.clone())?;
                }
                Ok(())
            }
        },
        Exp::FunDefs { decls, body, .. } => {
            // mutual recursion this time(!)
            let mut names: HashMap<&str, usize> = HashMap::new();
            for decl in decls {
                if names.contains_key(decl.name.as_str()) {
                    return Err(CompileErr::DuplicateFunName {
                        duplicated_name: String::from(&decl.name),
                        location: decl.ann.clone(),
                    });
                }
                names.insert(decl.name.as_str(), decl.parameters.len());
            }
            for (name, size) in names.into_iter() {
                env.push_fun(name, size);
            }
            for decl in decls {
                check_decl(decl, env.clone())?;
            }
            check_exp(body, env)?;
            Ok(())
        }
        Exp::InternalTailCall(..) | Exp::ExternalCall { .. } => {
            panic!("should never happen in check_prog")
        }
        Exp::Semicolon { e1, e2, ann:_ } => {
            check_exp(&e1, env.clone());
            check_exp(&e2, env.clone())
        }
    }
}

pub fn check_prog<Span>(p: &SurfProg<Span>) -> Result<(), CompileErr<Span>>
where
    Span: Clone,
{
    check_exp(p, SurfEnv::new())
}

fn uniquify(e: &Exp<u32>) -> Exp<()> {
    fn uniq_loop<'exp>(e: &'exp Exp<u32>, mut env: SurfEnv<'exp, String, String>) -> Exp<()> {
        match e {
            Exp::InternalTailCall(..) | Exp::ExternalCall { .. } => {
                panic!("should never happen in uniquify")
            }
            Exp::Num(n, _) => Exp::Num(*n, ()),
            Exp::Bool(b, _) => Exp::Bool(*b, ()),
            Exp::Char(c, _) => Exp::Char(*c, ()),
            Exp::Prim(op, es, _) => Exp::Prim(
                *op,
                es.iter()
                    .map(|e| Box::new(uniq_loop(e, env.clone())))
                    .collect(),
                (),
            ),
            Exp::If { cond, thn, els, .. } => Exp::If {
                cond: Box::new(uniq_loop(cond, env.clone())),
                thn: Box::new(uniq_loop(thn, env.clone())),
                els: Box::new(uniq_loop(els, env.clone())),
                ann: (),
            },

            // Interesting cases: using a value/function variable
            Exp::Var(v, _) => Exp::Var(env.get_var(v).unwrap(), ()),
            Exp::Call(f, args, _) => {
                let f2 = env.get_fun(f).unwrap();
                let args2 = args.iter().map(|e| uniq_loop(e, env.clone())).collect();
                Exp::Call(f2, args2, ())
            }

            // Declaring value/function variables
            Exp::Let {
                bindings,
                body,
                ann: noise,
            } => {
                let new_bindings = bindings
                    .iter()
                    .map(|(x, e)| {
                        let new_x = format!("{}#{}", x, noise);
                        let new_e = uniq_loop(e, env.clone());
                        env.push_var(x, new_x.clone());
                        (new_x, new_e)
                    })
                    .collect();
                Exp::Let {
                    bindings: new_bindings,
                    body: Box::new(uniq_loop(body, env)),
                    ann: (),
                }
            }
            Exp::FunDefs {
                decls,
                body,
                ann: noise,
            } => {
                for decl in decls.iter() {
                    let new_f = format!("{}#{}", decl.name, noise);
                    env.push_fun(&decl.name, new_f);
                }
                let new_decls = decls
                    .iter()
                    .map(|decl| {
                        let new_f = format!("{}#{}", decl.name, noise);
                        let mut local_env = env.clone();
                        let new_parameters = decl
                            .parameters
                            .iter()
                            .map(|parameter| {
                                let new_parameter = format!("{}#{}", parameter, noise);
                                local_env.push_var(parameter, new_parameter.clone());
                                new_parameter
                            })
                            .collect();
                        FunDecl {
                            name: new_f,
                            parameters: new_parameters,
                            body: uniq_loop(&decl.body, local_env),
                            ann: (),
                        }
                    })
                    .collect();
                Exp::FunDefs {
                    decls: new_decls,
                    body: Box::new(uniq_loop(body, env)),
                    ann: (),
                }
            }
            Exp::Semicolon { e1, e2, ann: _ } => Exp::Semicolon { 
                e1: Box::new(uniq_loop(e1, env.clone())), 
                e2:  Box::new(uniq_loop(e2, env.clone())), 
                ann: () 
            },
        }
    }
    uniq_loop(e, SurfEnv::new())
}

// Analyzes an expression to determine which functions within it need
// to be lambda lifted.

// What should be lambda lifted?
// 1. Any function that is called with a non-tail call.
// 2. Any function that is live in another function that is lambda lifted.
// Oversimplification of 2: any function that is in scope in another function that is lambda lifted.
fn should_lifts<Ann>(p: &Exp<Ann>) -> HashSet<String> {
    fn sl<'e, Ann>(
        p: &'e Exp<Ann>,
        mut env: SurfEnv<'e, Vec<&'e str>, ()>,
        shoulds: &mut HashSet<String>,
        is_tail: bool,
    ) {
        match p {
            Exp::Num(..) | Exp::Bool(..) | Exp::Char(..) | Exp::Var(..) => {}
            Exp::Prim(_, es, _) => {
                for e in es {
                    sl(e, env.clone(), shoulds, false)
                }
            }
            Exp::Let { bindings, body, .. } => {
                for (_, rhs) in bindings {
                    sl(rhs, env.clone(), shoulds, false);
                }
                sl(body, env, shoulds, is_tail)
            }
            Exp::If { cond, thn, els, .. } => {
                sl(cond, env.clone(), shoulds, false);
                sl(thn, env.clone(), shoulds, is_tail);
                sl(els, env.clone(), shoulds, is_tail);
            }
            Exp::FunDefs { decls, body, .. } => {
                let mut live_funs = env.all_funs();
                live_funs.extend(decls.iter().map(|d| d.name.as_str()));
                for decl in decls {
                    {
                        env.push_fun(&decl.name, live_funs.clone());
                    }
                }
                for decl in decls {
                    sl(&decl.body, env.clone(), shoulds, true)
                }
                sl(body, env, shoulds, is_tail)
            }
            // the only interesting case
            Exp::Call(f, args, _) => {
                for arg in args {
                    sl(arg, env.clone(), shoulds, false)
                }
                if !is_tail {
                    for g in env.get_fun(f).unwrap() {
                        shoulds.insert(g.to_string());
                    }
                }
            }
            Exp::InternalTailCall(..) | Exp::ExternalCall { .. } => {
                unreachable!()
            }
            Exp::Semicolon { e1, e2, ann:_ } => {
                sl(e1, env.clone(), shoulds, is_tail);
                sl(e2, env.clone(), shoulds, is_tail)
            }
        }
    }
    let mut hs = HashSet::new();
    sl(p, SurfEnv::new(), &mut hs, true);
    hs
}

// Precondition: all name bindings are unique
fn lambda_lift<Ann>(p: &Exp<Ann>) -> (Vec<FunDecl<Exp<()>, ()>>, Exp<()>) {
    fn ll<'exp, Ann>(
        should_be_lifted: &HashSet<String>,
        e: &'exp Exp<Ann>,
        o_decls: &mut Vec<FunDecl<Exp<()>, ()>>,
        mut env: SurfEnv<'exp, Vec<String>, ()>,
        tail_position: bool,
    ) -> Exp<()> {
        match e {
            // Boring cases
            Exp::Num(n, _) => Exp::Num(*n, ()),
            Exp::Bool(b, _) => Exp::Bool(*b, ()),
            Exp::Char(c, _) => Exp::Char(*c, ()),
            Exp::Var(v, _) => Exp::Var(v.clone(), ()),
            Exp::Prim(op, es, _) => Exp::Prim(
                *op,
                es.iter()
                    .map(|e| Box::new(ll(should_be_lifted, e, o_decls, env.clone(), false)))
                    .collect(),
                (),
            ),
            Exp::If { cond, thn, els, .. } => Exp::If {
                cond: Box::new(ll(should_be_lifted, cond, o_decls, env.clone(), false)),
                thn: Box::new(ll(
                    should_be_lifted,
                    thn,
                    o_decls,
                    env.clone(),
                    tail_position,
                )),
                els: Box::new(ll(
                    should_be_lifted,
                    els,
                    o_decls,
                    env.clone(),
                    tail_position,
                )),
                ann: (),
            },
            Exp::Let { bindings, body, .. } => {
                let bindings = bindings
                    .iter()
                    .map(|(x, rhs)| {
                        let e = ll(should_be_lifted, rhs, o_decls, env.clone(), false);
                        env.push_var(x, ());
                        (x.clone(), e)
                    })
                    .collect();
                Exp::Let {
                    bindings,
                    body: Box::new(ll(should_be_lifted, body, o_decls, env, tail_position)),
                    ann: (),
                }
            }
            Exp::InternalTailCall(..) | Exp::ExternalCall { .. } => {
                panic!("should never happen: internaltailcall/externalcall during Lambda Lifting")
            }
            // Interesting cases
            Exp::Call(f, args, _) => {
                if should_be_lifted.contains(f) {
                    let mut new_args: Vec<Exp<()>> = env
                        .get_fun(f)
                        .unwrap()
                        .into_iter()
                        .map(|x| Exp::Var(x.clone(), ()))
                        .collect();
                    new_args.extend(
                        args.iter()
                            .map(|e| ll(should_be_lifted, e, o_decls, env.clone(), false)),
                    );
                    Exp::ExternalCall {
                        fun_name: f.clone(),
                        args: new_args,
                        is_tail: tail_position,
                        ann: (),
                    }
                } else if !tail_position {
                    panic!("non-tail call to a function that hasn't been lambda lifted")
                } else {
                    Exp::InternalTailCall(
                        f.clone(),
                        args.iter()
                            .map(|e| ll(should_be_lifted, e, o_decls, env.clone(), false))
                            .collect(),
                        (),
                    )
                }
            }
            Exp::FunDefs { decls, body, .. } => {
                // Most interesting case :)
                // first we figure out what the current variables are,
                let free_vars: Vec<String> = env.all_vars().into_iter().map(String::from).collect();

                // then we extend the environment to include these
                // mutually recursive functions mapping to their new vars
                for decl in decls {
                    if should_be_lifted.contains(&decl.name) {
                        env.push_fun(&decl.name, free_vars.clone());
                    }
                }

                let mut unlifted_decls: Vec<FunDecl<Exp<()>, ()>> = Vec::new();

                // then we output the extended funs into the output vector
                for decl in decls {
                    if should_be_lifted.contains(&decl.name) {
                        let mut new_params = free_vars.clone();
                        new_params.extend(decl.parameters.iter().map(String::from));
                        let mut local_env = env.clone();
                        for param in new_params.iter() {
                            local_env.push_var(param, ());
                        }
                        let fd = FunDecl {
                            name: decl.name.clone(),
                            parameters: new_params.clone(),
                            body: ll(should_be_lifted, &decl.body, o_decls, local_env, true),
                            ann: (),
                        };
                        o_decls.push(fd);
                    } else {
                        let fd = FunDecl {
                            name: decl.name.clone(),
                            parameters: decl.parameters.clone(),
                            body: ll(should_be_lifted, &decl.body, o_decls, env.clone(), true),
                            ann: (),
                        };
                        unlifted_decls.push(fd);
                    }
                }

                // and we return the updated body
                Exp::FunDefs {
                    decls: unlifted_decls,
                    body: Box::new(ll(should_be_lifted, body, o_decls, env, tail_position)),
                    ann: (),
                }
            },
            Exp::Semicolon { e1, e2, ann:_ } => Exp::Semicolon { 
                e1: Box::new(ll(should_be_lifted, e1, o_decls, env.clone(), tail_position,)), 
                e2: Box::new(ll(should_be_lifted, e2, o_decls, env.clone(), tail_position,)), 
                ann: () 
            },
        }
    }
    let should_be_lifted = should_lifts(p);

    let mut v = Vec::new();
    let e = ll(&should_be_lifted, p, &mut v, SurfEnv::new(), true);
    (v, e)
}

fn tag_exp<Ann>(p: &SurfProg<Ann>) -> SurfProg<u32> {
    let mut i = 0;
    p.map_ann(
        &mut (|_| {
            let cur = i;
            i += 1;
            cur
        }),
    )
}

fn tag_prog<Ann>(
    defs: &[FunDecl<Exp<Ann>, Ann>],
    main: &Exp<Ann>,
) -> (Vec<FunDecl<Exp<u32>, u32>>, Exp<u32>) {
    let mut i = 0;
    (
        defs.iter()
            .map(|decl| {
                decl.map_ann(
                    &mut (|_| {
                        let cur = i;
                        i += 1;
                        cur
                    }),
                )
            })
            .collect(),
        main.map_ann(
            &mut (|_| {
                let cur = i;
                i += 1;
                cur
            }),
        ),
    )
}

fn tag_sprog<Ann>(p: &SeqProg<Ann>) -> SeqProg<u32> {
    let mut i = 0;
    p.map_ann(
        &mut (|_| {
            let cur = i;
            i += 1;
            cur
        }),
    )
}

fn sequentialize(e: &Exp<u32>) -> SeqExp<()> {
    match e {
        Exp::Num(i, _) => SeqExp::Imm(ImmExp::Num(*i), ()),
        Exp::Bool(b, _) => SeqExp::Imm(ImmExp::Bool(*b), ()),
        Exp::Char(c, _) => SeqExp::Imm(ImmExp::Char(*c), ()),
        Exp::Var(x, _) => SeqExp::Imm(ImmExp::Var(x.clone()), ()),
        Exp::Prim(op, es, ann) => {
            let vars: Vec<String> = es
                .iter()
                .enumerate()
                .map(|(sz, _)| format!("prim_var#{}#{}", sz, ann))
                .collect();
            let mut body = SeqExp::Prim(
                *op,
                vars.iter().map(|x| ImmExp::Var(x.clone())).collect(),
                (),
            );
            for (var, e) in vars.into_iter().zip(es).rev() {
                body = SeqExp::Let {
                    var,
                    bound_exp: Box::new(sequentialize(e)),
                    body: Box::new(body),
                    ann: (),
                }
            }
            body
        }
        Exp::Let { bindings, body, .. } => {
            let mut s_e = sequentialize(body);
            for (x, e) in bindings.iter().rev() {
                s_e = SeqExp::Let {
                    var: x.clone(),
                    bound_exp: Box::new(sequentialize(e)),
                    body: Box::new(s_e),
                    ann: (),
                }
            }
            s_e
        }
        Exp::If {
            cond,
            thn,
            els,
            ann: tag,
        } => {
            let s_cond = sequentialize(cond);
            let name = format!("#if_{}", tag);
            SeqExp::Let {
                var: name.clone(),
                bound_exp: Box::new(s_cond),
                body: Box::new(SeqExp::If {
                    cond: ImmExp::Var(name),
                    thn: Box::new(sequentialize(thn)),
                    els: Box::new(sequentialize(els)),
                    ann: (),
                }),
                ann: (),
            }
        }
        Exp::InternalTailCall(name, args, tag) => {
            let arg_vars: Vec<String> = (0..args.len())
                .map(|i| format!("#internal_tail_call_arg_{}_{}", tag, i))
                .collect();
            let mut body = SeqExp::InternalTailCall(
                name.clone(),
                arg_vars.iter().map(|x| ImmExp::Var(x.clone())).collect(),
                (),
            );
            for (arg_e, arg_var) in args.iter().zip(arg_vars).rev() {
                body = SeqExp::Let {
                    var: arg_var,
                    bound_exp: Box::new(sequentialize(arg_e)),
                    body: Box::new(body),
                    ann: (),
                };
            }
            body
        }
        Exp::ExternalCall {
            fun_name,
            args,
            is_tail,
            ann: tag,
        } => {
            let arg_vars: Vec<String> = (0..args.len())
                .map(|i| format!("#external_call_arg_{}_{}", tag, i))
                .collect();
            let mut body = SeqExp::ExternalCall {
                fun_name: fun_name.clone(),
                args: arg_vars.iter().map(|x| ImmExp::Var(x.clone())).collect(),
                is_tail: *is_tail,
                ann: (),
            };
            for (arg_e, arg_var) in args.iter().zip(arg_vars).rev() {
                body = SeqExp::Let {
                    var: arg_var,
                    bound_exp: Box::new(sequentialize(arg_e)),
                    body: Box::new(body),
                    ann: (),
                };
            }
            body
        }
        Exp::Call(..) => {
            panic!("Call shouldn't reach sequentialize")
        }
        Exp::FunDefs { decls, body, .. } => SeqExp::FunDefs {
            decls: decls
                .iter()
                .map(|d| FunDecl {
                    name: d.name.clone(),
                    parameters: d.parameters.clone(),
                    body: sequentialize(&d.body),
                    ann: (),
                })
                .collect(),
            body: Box::new(sequentialize(body)),
            ann: (),
        },
        Exp::Semicolon { e1, e2, ann:_ } => SeqExp::Semicolon { 
            e1: Box::new(sequentialize(e1)), 
            e2: Box::new(sequentialize(e2)), 
            ann: () 
        }
    }
}

fn seq_prog(decls: &[FunDecl<Exp<u32>, u32>], p: &Exp<u32>) -> SeqProg<()> {
    SeqProg {
        funs: decls
            .iter()
            .map(|d| FunDecl {
                name: d.name.clone(),
                parameters: d.parameters.clone(),
                body: sequentialize(&d.body),
                ann: (),
            })
            .collect(),
        main: sequentialize(p),
        ann: (),
    }
}

static SNAKE_TRUE: u64 = 0xFF_FF_FF_FF_FF_FF_FF_FF;
static SNAKE_FALSE: u64 = 0x7F_FF_FF_FF_FF_FF_FF_FF;
static NOT_MASK: u64 = 0x80_00_00_00_00_00_00_00;

fn compile_imm(e: &ImmExp, env: &CodeGenEnv) -> Arg64 {
    match e {
        ImmExp::Num(i) => Arg64::Signed(*i << 1),
        ImmExp::Bool(b) => Arg64::Unsigned(if *b { SNAKE_TRUE } else { SNAKE_FALSE }),
        //leftshift 3 and tag 101 (5)imm
        ImmExp::Char(c) => Arg64::Signed(((*c as i64) << 3) + 5),
        ImmExp::Var(x) => Arg64::Mem(MemRef {
            reg: Reg::Rsp,
            offset: env
                .lookup_local_offset(x)
                .unwrap_or_else(|| panic!("didn't find {}", x)),
        }),
    }
}

fn cmp_args<F>(args: BinArgs, cond_jmp: F, tag: u32) -> Vec<Instr>
where
    F: FnOnce(String) -> Instr,
{
    let tru_lab = format!("cmp_true#{}", tag);
    let done_lab = format!("cmp_done#{}", tag);
    vec![
        Instr::Cmp(args),
        cond_jmp(tru_lab.clone()),
        Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(SNAKE_FALSE))),
        Instr::Jmp(done_lab.clone()),
        Instr::Label(tru_lab),
        Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(SNAKE_TRUE))),
        Instr::Label(done_lab),
    ]
}

fn num_test(arg: Arg64, lab: &str) -> Vec<Instr> {
    vec![
        Instr::Mov(MovArgs::ToReg(Reg::Rsi, arg)),
        Instr::Test(BinArgs::ToReg(Reg::Rsi, Arg32::Signed(1))),
        Instr::Jnz(String::from(lab)),
    ]
}

fn bool_test(arg: Arg64, lab: &str) -> Vec<Instr> {
    vec![
        Instr::Mov(MovArgs::ToReg(Reg::Rsi, arg)),
        Instr::Test(BinArgs::ToReg(Reg::Rsi, Arg32::Signed(0b1))),
        Instr::Jz(String::from(lab)),
        Instr::Test(BinArgs::ToReg(Reg::Rsi, Arg32::Signed(0b10))),
        Instr::Jz(String::from(lab)),
        Instr::Test(BinArgs::ToReg(Reg::Rsi, Arg32::Signed(0b100))),
        Instr::Jz(String::from(lab)),
    ]
}

fn prim2_num_checks(arg1: Arg64, arg2: Arg64, lab: &str) -> Vec<Instr> {
    let mut is = num_test(arg1, lab);
    is.extend(num_test(arg2, lab));
    is
}
fn prim2_bool_checks(arg1: Arg64, arg2: Arg64, lab: &str) -> Vec<Instr> {
    let mut is = bool_test(arg1, lab);
    is.extend(bool_test(arg2, lab));
    is
}

fn user_fun_to_label(s: &str) -> String {
    format!("user_{}", s)
}

fn mov_to_mem64(mem: MemRef, arg: Arg64) -> Vec<Instr> {
    match arg {
        Arg64::Reg(r) => vec![Instr::Mov(MovArgs::ToMem(mem, Reg32::Reg(r)))],
        Arg64::Mem(mem_from) => vec![
            Instr::Mov(MovArgs::ToReg(SCRATCH, Arg64::Mem(mem_from))),
            Instr::Mov(MovArgs::ToMem(mem, Reg32::Reg(SCRATCH))),
        ],
        Arg64::Signed(n) => {
            vec![
                Instr::Mov(MovArgs::ToReg(SCRATCH, Arg64::Signed(n))),
                Instr::Mov(MovArgs::ToMem(mem, Reg32::Reg(SCRATCH))),
            ]
        }
        Arg64::Unsigned(n) => {
            vec![
                Instr::Mov(MovArgs::ToReg(SCRATCH, Arg64::Unsigned(n))),
                Instr::Mov(MovArgs::ToMem(mem, Reg32::Reg(SCRATCH))),
            ]
        }
    }
}

#[derive(Clone, Debug)]
struct CodeGenEnv<'e> {
    bb_offsets: List<(&'e str, i32)>,
    local_offsets: List<(&'e str, i32)>,
    size: i32,
}

impl<'e> CodeGenEnv<'e> {
    fn new(params: Vec<(&'e str, i32)>) -> CodeGenEnv<'e> {
        CodeGenEnv {
            bb_offsets: List::new(),
            local_offsets: params.into_iter().collect(),
            size: 0,
            // next: -8,
        }
    }

    fn push_fun(&mut self, f: &'e str) {
        self.bb_offsets.push((f, self.size))
    }

    // if currently I have size variables, the next variable will be stored at offset rsp - 8 * size
    fn push(&mut self, x: &'e str) -> i32 {
        self.size += 1;
        let new = self.size * -8;
        self.local_offsets.push((x, new));
        new
    }

    fn lookup_local_offset(&self, x: &str) -> Option<i32> {
        for (y, off) in self.local_offsets.iter() {
            if x == y {
                return Some(off);
            }
        }
        None
    }

    fn lookup_fun_offset(&self, f: &str) -> Option<i32> {
        for (x, off) in self.bb_offsets.iter() {
            if x == f {
                return Some(off);
            }
        }
        None
    }
}

fn round_up_even(i: i32) -> i32 {
    i + (i % 2)
}

fn round_up_odd(i: i32) -> i32 {
    i + ((i + 1) % 2)
}

fn compile_with_env<'exp>(
    e: &'exp SeqExp<u32>,      // the expression to be compiled
    mut env: CodeGenEnv<'exp>, // the environment mapping variables to their location on the stack and local functions to the offset to their arguments on the stack
) -> Vec<Instr> {
    match e {
        SeqExp::Imm(imm, _) => vec![Instr::Mov(MovArgs::ToReg(Reg::Rax, compile_imm(imm, &env)))],
        SeqExp::Semicolon { e1, e2, ann } => { 

            let mut is = compile_with_env(e1, env.clone());

            is.extend(compile_with_env(e2, env));

            is
        },
        SeqExp::Prim(op, imms, tag) => match op {

            //new Prims
            Prim::HeapGet => {

                let mut arg = compile_imm(&imms[0], &env);
                let mut is = vec![];

                is.push(Instr::Comment(String::from("Heap Get")));

                //load variable into R8
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R8, arg)));

                //check if our variable is an array

                //checking 0b<0><b>1
                is.push(Instr::Test(BinArgs::ToReg( Reg::R8, Arg32::Signed( 0b001 ) )));
                is.push(Instr::Jz(String::from("index_non_both")));

                //checking 0b0b<1>
                is.push(Instr::Test(BinArgs::ToReg( Reg::R8, Arg32::Signed( 0b100 ) )));
                is.push(Instr::Jnz(String::from("index_non_both")));


                //get rid of tag bit
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Unsigned(0xFF_FF_FF_FF_FF_FF_FF_F8))));

                is.push(Instr::And( BinArgs::ToReg( Reg::R8, Arg32::Reg(Reg::Rsi) ) ) );
                

                //pull the length from the heap
                is.push( Instr::Mov( MovArgs::ToReg( Reg::R8, Arg64::Mem(MemRef { reg: Reg::R8, offset: 0 }))));


                //shift the number one to the left to fit into our snake tagging convention
                is.push(Instr::Shl( BinArgs::ToReg(Reg::R8, Arg32::Signed(1)) ));



                //overflow
                is.push(Instr::Jo(String::from("overflow_err") ));

                //check if we're going out of bounrd with our index
                arg = compile_imm(&imms[1], &env);

                

                //load index into R9 for comparison
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, arg.clone())));


                //check if index is a number
                is.extend(vec![Instr::Mov(MovArgs::ToReg(Reg::Rsi, arg)),
                    Instr::Test(BinArgs::ToReg(Reg::Rsi, Arg32::Signed(1))),
                    Instr::Jnz(String::from("index_nan"))   ]);

                //compare index (R9) to size of array (R8)
                is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Reg(Reg::R8))));

                //jump to error if applicable
                is.push(Instr::Jge( String::from("index_oob_err") ));

                //jumping if indexing w negative #
                is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Signed(0))));
                is.push(Instr::Jl( String::from("index_oob_err") ));

                //convert the offset into a multiple of 8 (w/o tag bit) and add 8 for getting to index[0]
                //multiply by 4 (left shift already multiplies by 2)

                is.push(Instr::IMul(BinArgs::ToReg(Reg::R9, Arg32::Signed(4))));

                //add 8
                is.push(Instr::Add(BinArgs::ToReg(Reg::R9, Arg32::Signed(8))));

                //load array location back into R8
                arg = compile_imm(&imms[0], &env);
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R8, arg)));

                //get rid of tag bit
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Unsigned(0xFF_FF_FF_FF_FF_FF_FF_F8))));

                is.push(Instr::And( BinArgs::ToReg( Reg::R8, Arg32::Reg(Reg::Rsi) ) ) );




                //add to pointer (stored in R8)
                is.push(Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Reg(Reg::R9))));

                //pull ptr + offset value into RAX
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Mem( MemRef {reg: Reg::R8, offset: 0}) )));

                is

            },

            Prim::Length => {

                let arg = compile_imm(&imms[0], &env);
                let mut is = vec![];

                //load variable into RAX
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, arg)));

                //check if our variable is an array/string

                //checking 0b<0>?1
                is.push(Instr::Test(BinArgs::ToReg( Reg::Rax, Arg32::Signed( 0b100 ) )));
                is.push(Instr::Jnz(String::from("length_not_array_error")));

                //checking 0b0?<1>
                is.push(Instr::Test(BinArgs::ToReg( Reg::Rax, Arg32::Signed( 0b001 ) )));
                is.push(Instr::Jz(String::from("length_not_array_error")));

                //get rid of tag bit
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Unsigned(0xFF_FF_FF_FF_FF_FF_FF_F8))));
                is.push(Instr::And(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::Rsi))));

                //pull the length from the heap
                is.push( Instr::Mov( MovArgs::ToReg( Reg::Rax, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }))));

                //shift the number one to the left to fit into our snake tagging convention
                is.push(Instr::Shl( BinArgs::ToReg(Reg::Rax, Arg32::Signed(1)) ));

                //overflow
                is.push(Instr::Jo( String::from("overflow_err") ));

                is

            },

            Prim::HeapSet => {

                let mut arg = compile_imm(&imms[0], &env);
                let mut is = vec![];

                //load variable into RAX
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, arg)));

                //check if our variable is an array

                //checking 0b<0>?1
                is.push(Instr::Test(BinArgs::ToReg( Reg::Rax, Arg32::Signed( 0b100 ) )));
                is.push(Instr::Jnz(String::from("index_non_both")));

                //checking 0b0?<1>
                is.push(Instr::Test(BinArgs::ToReg( Reg::Rax, Arg32::Signed( 0b001 ) )));
                is.push(Instr::Jz(String::from("index_non_both")));

                //checking 0b<?>1 -> ? = 1 if String, 0 if Array
                is.push(Instr::Test(BinArgs::ToReg( Reg::Rax, Arg32::Signed( 0b010 ) )));
                is.push(Instr::Jnz(String::from(format!("StringSet{}", tag))));

                //ARRAY LOGIC

                //get rid of tag bit
                is.push(Instr::Sub( BinArgs::ToReg( Reg::Rax, Arg32::Signed(1) ) ) );

                //pull the length from the heap and store in R8
                is.push( Instr::Mov( MovArgs::ToReg( Reg::R8, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }))));

                //shift the number one to the left to fit into our snake tagging convention
                is.push(Instr::Shl( BinArgs::ToReg(Reg::R8, Arg32::Signed(1)) ));

                //overflow
                is.push(Instr::Jo( String::from("overflow_err") ));

                //check if we're going out of bounds with our index
                arg = compile_imm(&imms[1], &env);

                //load index into R9 for comparison
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, arg.clone())));

                //check if index is a number
                is.extend(vec![Instr::Mov(MovArgs::ToReg(Reg::Rsi, arg)),
                    Instr::Test(BinArgs::ToReg(Reg::Rsi, Arg32::Signed(1))),
                    Instr::Jnz(String::from("index_nan"))   ]);


                //compare index (R9) to size of array (R8)
                is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Reg(Reg::R8))));

                //jump to error if applicable
                is.push(Instr::Jge( String::from("index_oob_err") ));
                
                //if index less than 0
                is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Signed(0))));
                is.push(Instr::Jl(String::from("index_oob_err")));

                //convert the offset into a multiple of 8 (w/o tag bit) and add 8 for getting to index[0]
                //multiply by 4 (left shift already multiplies by 2)
                is.push(Instr::IMul(BinArgs::ToReg(Reg::R9, Arg32::Signed(4))));

                //add 8
                is.push(Instr::Add(BinArgs::ToReg(Reg::R9, Arg32::Signed(8))));

                //load array location back into R8
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R8, Arg64::Reg(Reg::Rax))));

                //add to pointer (stored in R8)
                is.push(Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Reg(Reg::R9))));

                //store our third arg in R9
                arg = compile_imm(&imms[2], &env);
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, arg.clone())));

                //write R9 into memory location stored at R8
                is.push(Instr::Mov( MovArgs::ToMem(MemRef { reg: Reg::R8, offset: 0 }, Reg32::Reg(Reg::R9) )));

                //retagging
                is.push(Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Signed(1))));

                //jmp to done
                is.push(Instr::Jmp(String::from(format!("HeapSetDone{}", tag))));

                //**********************************************/
                //STRING LOGIC
                //**********************************************/

                is.push(Instr::Label(String::from(format!("StringSet{}", tag))));

                //get rid of tag bits
                is.push(Instr::Sub( BinArgs::ToReg( Reg::Rax, Arg32::Signed(3) ) ) );

                //pull the length from the heap and store in R8
                is.push( Instr::Mov( MovArgs::ToReg( Reg::R8, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }))));

                //shift the number one to the left to fit into our snake tagging convention
                is.push(Instr::Shl( BinArgs::ToReg(Reg::R8, Arg32::Signed(1)) ));

                //overflow
                is.push(Instr::Jo( String::from("overflow_err") ));

                //check if we're going out of bounds with our index
                arg = compile_imm(&imms[1], &env);

                //load index into R9 for comparison
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, arg.clone())));

                //check if index is a number
                is.extend(vec![Instr::Mov(MovArgs::ToReg(Reg::Rsi, arg)),
                    Instr::Test(BinArgs::ToReg(Reg::Rsi, Arg32::Signed(1))),
                    Instr::Jnz(String::from("index_nan"))   ]);


                //compare index (R9) to size of array (R8)
                is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Reg(Reg::R8))));

                //jump to error if applicable
                is.push(Instr::Jge( String::from("index_oob_err") ));
                
                //if index less than 0
                is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Signed(0))));
                is.push(Instr::Jl(String::from("index_oob_err")));

                //convert the offset into a multiple of 8 (w/o tag bit) and add 8 for getting to index[0]
                //multiply by 4 (left shift already multiplies by 2)
                is.push(Instr::IMul(BinArgs::ToReg(Reg::R9, Arg32::Signed(4))));

                //add 8
                is.push(Instr::Add(BinArgs::ToReg(Reg::R9, Arg32::Signed(8))));

                //load array location back into R8
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R8, Arg64::Reg(Reg::Rax))));

                //add to pointer (stored in R8)
                is.push(Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Reg(Reg::R9))));

                //store our third arg in R9
                arg = compile_imm(&imms[2], &env);
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, arg.clone())));

                //check if R9 is a char
                //checking 0b<0>01
                is.push(Instr::Test(BinArgs::ToReg( Reg::R9, Arg32::Signed( 0b100 ) )));
                is.push(Instr::Jz(String::from("set_non_char")));

                //checking 0b0<1>1
                is.push(Instr::Test(BinArgs::ToReg( Reg::R9, Arg32::Signed( 0b010 ) )));
                is.push(Instr::Jnz(String::from("set_non_char")));

                //checking 0b01<1>
                is.push(Instr::Test(BinArgs::ToReg( Reg::R9, Arg32::Signed( 0b001 ) )));
                is.push(Instr::Jz(String::from("set_non_char")));


                //write R9 into memory location stored at R8
                is.push(Instr::Mov( MovArgs::ToMem(MemRef { reg: Reg::R8, offset: 0 }, Reg32::Reg(Reg::R9) )));

                //retagging
                is.push(Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Signed(3))));


                //**********************************************/

                is.push(Instr::Label(String::from(format!("HeapSetDone{}", tag))));
                is
                
            },

            Prim::IsChar => {

                let mut is = vec![];

                let arg = compile_imm(&imms[0], &env);

                // say l is the num locals
                // rsp is the base.
                // last local is at [RSP - 8 * l]
                //
                // 0th arg is at
                // mov [rax - (8 * (l + 2 + 0))]
                let l = round_up_odd(env.size);

                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rdi, arg)));
                
                is.push(Instr::Sub(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(8 * l))));
                is.push(Instr::Call(String::from("ischar")));
                is.push(Instr::Add(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(8 * l))));


                is
                
            },

            Prim::IsArray => {

                let mut is = vec![];

                let arg = compile_imm(&imms[0], &env);

                // say l is the num locals
                // rsp is the base.
                // last local is at [RSP - 8 * l]
                //
                // 0th arg is at
                // mov [rax - (8 * (l + 2 + 0))]
                let l = round_up_odd(env.size);

                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rdi, arg)));
                
                is.push(Instr::Sub(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(8 * l))));
                is.push(Instr::Call(String::from("isarray")));
                is.push(Instr::Add(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(8 * l))));


                is
                
            },

            Prim::IsString => {

                let mut is = vec![];

                let arg = compile_imm(&imms[0], &env);

                // say l is the num locals
                // rsp is the base.
                // last local is at [RSP - 8 * l]
                //
                // 0th arg is at
                // mov [rax - (8 * (l + 2 + 0))]
                let l = round_up_odd(env.size);

                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rdi, arg)));
                
                is.push(Instr::Sub(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(8 * l))));
                is.push(Instr::Call(String::from("isstring")));
                is.push(Instr::Add(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(8 * l))));


                is
                
            },

            Prim::Range => {

                let mut is = vec![];
                let mut arg0: Arg64 = compile_imm(&imms[0], &env);
                let mut arg1: Arg64 = compile_imm(&imms[1], &env);
                let mut arg2: Arg64 = compile_imm(&imms[2], &env);

                //saving R15 into RAX
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::R15))));

                //pulling tag bits from arg0
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R10, arg0)));
                
                //checking if lsb is a 1 (must be if its an array or a string)
                is.push(Instr::Test((BinArgs::ToReg(Reg::R10, Arg32::Signed(1)))));
                is.push(Instr::Jz(String::from("index_non_both")));

                //checking if 3rd bit is a 0 (must be if its an array or a string)
                is.push(Instr::Test((BinArgs::ToReg(Reg::R10, Arg32::Signed(0b100)))));
                is.push(Instr::Jnz(String::from("index_non_both")));

                //We now know that R10 is a string/array

                //removing tag bit
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, Arg64::Unsigned(0xFF_FF_FF_FF_FF_FF_FF_F8))));
                is.push(Instr::And(BinArgs::ToReg(Reg::R10, Arg32::Reg(Reg::R9))));

                //loading length of R10 into R9
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, Arg64::Mem(MemRef { reg: Reg::R10, offset: 0 }))));

                //left shift the length
                is.push(Instr::Shl(BinArgs::ToReg(Reg::R9, Arg32::Signed(1))));

                //load idx1 into RDI, idx2 into RSI
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rdi, arg1)));
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rsi, arg2)));

                //make sure indexes are numbers
                is.push(Instr::Test(BinArgs::ToReg(Reg::Rdi, Arg32::Signed(1))));
                is.push(Instr::Jnz(String::from("index_nan")));
                is.push(Instr::Test(BinArgs::ToReg(Reg::Rsi, Arg32::Signed(1))));
                is.push(Instr::Jnz(String::from("index_nan")));

                //check if idx2 is geq size
                is.push(Instr::Cmp(BinArgs::ToReg(Reg::Rsi, Arg32::Reg(Reg::R9))));
                is.push(Instr::Jge(String::from("index_oob_err")));

                //check if idx1 is lt 0
                is.push(Instr::Cmp(BinArgs::ToReg(Reg::Rdi, Arg32::Signed(0))));
                is.push(Instr::Jl(String::from("index_oob_err")));

                //check if idx2 is lt idx1
                is.push(Instr::Cmp(BinArgs::ToReg(Reg::Rsi, Arg32::Reg(Reg::Rdi))));
                is.push(Instr::Jl(String::from("range_dec_err")));

                //making our new object

                //making the size
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R8, Arg64::Reg(Reg::Rsi))));
                is.push(Instr::Sub(BinArgs::ToReg(Reg::R8, Arg32::Reg(Reg::Rdi))));
                //right shifting to fit sizing convention
                is.push(Instr::Shr(BinArgs::ToReg(Reg::R8, Arg32::Signed(1))));
                //add 1 since you can't have a 0 sized slice
                is.push(Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Signed(1))));

                //moving size onto the heap, incrementing R15
                is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 0 }, Reg32::Reg(Reg::R8))));
                is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed(8))));

                //converting idx1 into a memory location (only multiply by 4 due to tag bit)
                is.push(Instr::IMul(BinArgs::ToReg(Reg::Rdi, Arg32::Signed(4))));

                //add 8 to R10 to get it to index 0
                is.push(Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Signed(8))));

                //add RDI to R10 to get it to the proper index on the heap
                is.push(Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Reg(Reg::Rdi))));

                //loop
                is.push(Instr::Label(String::from(format!("Range_Loop#{}", tag))));

                //load current R10 into RSI and put it onto the heap
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Mem(MemRef { reg: Reg::R10, offset: 0 }))));
                is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 0 }, Reg32::Reg(Reg::Rsi))));

                //increment R10, R15, decrement R8
                is.push(Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Signed(8))));
                is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed(8))));
                is.push(Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Signed(-1))));

                //check if done looping
                is.push(Instr::Cmp(BinArgs::ToReg(Reg::R8, Arg32::Signed(0))));
                is.push(Instr::Jg((String::from(format!("Range_Loop#{}", tag)))));

                //get tag bit
                is.push(Instr::Mov(MovArgs::ToReg(Reg::R10, arg0)));
                is.push(Instr::And(BinArgs::ToReg(Reg::R10, Arg32::Signed(0b111))));

                //tag R15
                is.push(Instr::Or(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::R10))));

                is
            },

            Prim::MakeString => {

                let mut is = vec![];
                let mut args: Arg64;

                //push imms.size() onto the heap
                is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 0 }, Reg32::Imm(imms.len() as i32))));

                for i in imms.iter().enumerate(){

                    //this may cause an issue if it's not turning everything into chars
                    args = compile_imm(&i.1, &env);

                    //load args into scratch register
                    is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, args)));

                    is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 8 * (1 + i.0 as i32) }, (Reg32::Reg(Reg::R9)))));

                }

                //save R15 location into RAX
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::R15))));

                //move R15 to the next available space on the heap
                is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed((8 * (1 + imms.len() as i32))))));

                //tagging RAX as a string variable
                is.push(Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Signed(3))));


                is
                
            },

            Prim::MakeArray => {

                let mut is = vec![];
                let mut args: Arg64;

                //push imms.size() onto the heap
                is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 0 }, Reg32::Imm(imms.len() as i32))));

                for i in imms.iter().enumerate(){

                    args = compile_imm(&i.1, &env);

                    //load args into scratch register
                    is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, args)));

                    is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 8 * (1 + i.0 as i32) }, (Reg32::Reg(Reg::R9)))));

                }

                //save R15 location into RAX
                is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::R15))));

                //move R15 to the next available space on the heap
                is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed((8 * (1 + imms.len() as i32))))));

                //tagging RAX as an array variable
                is.push(Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Signed(1))));


                is
                
            },

            Prim::Add1 | Prim::Sub1 | Prim::Not | Prim::Print | Prim::IsBool | Prim::IsNum => {
                let imm = &imms[0];
                let arg = compile_imm(imm, &env);
                let mut is = vec![];
                match op {
                    Prim::Add1 => {
                        is.extend(num_test(arg, "arith_err"));
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::Rsi))));
                        is.extend(vec![
                            Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Signed(2))),
                            Instr::Jo(String::from("overflow_err")),
                        ])
                    }
                    Prim::Sub1 => {
                        is.extend(num_test(arg, "arith_err"));
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::Rsi))));
                        is.extend(vec![
                            Instr::Sub(BinArgs::ToReg(Reg::Rax, Arg32::Signed(2))),
                            Instr::Jo(String::from("overflow_err")),
                        ])
                    }
                    Prim::Not => {
                        is.extend(bool_test(arg, "log_err"));
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::Rsi))));
                        is.extend(vec![
                            Instr::Mov(MovArgs::ToReg(SCRATCH, Arg64::Unsigned(NOT_MASK))),
                            Instr::Xor(BinArgs::ToReg(Reg::Rax, Arg32::Reg(SCRATCH))),
                        ]);
                    }
                    Prim::IsBool => {
                        let tru_lab = format!("isbool_true#{}", tag);
                        let done_lab = format!("isnum_done#{}", tag);
                        is.extend(vec![
                            Instr::Mov(MovArgs::ToReg(Reg::Rax, arg)),
                            Instr::Test(BinArgs::ToReg(Reg::Rax, Arg32::Signed(1))),
                            Instr::Jnz(tru_lab.clone()),
                            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(SNAKE_FALSE))),
                            Instr::Jmp(done_lab.clone()),
                            Instr::Label(tru_lab),
                            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(SNAKE_TRUE))),
                            Instr::Label(done_lab),
                        ])
                    }
                    Prim::IsNum => {
                        let tru_lab = format!("isbool_true#{}", tag);
                        let done_lab = format!("isnum_done#{}", tag);
                        is.extend(vec![
                            Instr::Mov(MovArgs::ToReg(Reg::Rax, arg)),
                            Instr::Test(BinArgs::ToReg(Reg::Rax, Arg32::Signed(1))),
                            Instr::Jz(tru_lab.clone()),
                            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(SNAKE_FALSE))),
                            Instr::Jmp(done_lab.clone()),
                            Instr::Label(tru_lab),
                            Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Unsigned(SNAKE_TRUE))),
                            Instr::Label(done_lab),
                        ])
                    }
                    Prim::Print => is.extend(vec![
                        Instr::Mov(MovArgs::ToReg(Reg::Rdi, arg)),
                        Instr::Sub(BinArgs::ToReg(
                            Reg::Rsp,
                            Arg32::Signed(8 * round_up_even(env.size)),
                        )),
                        Instr::Call("print_snake_val".to_string()),
                        Instr::Add(BinArgs::ToReg(
                            Reg::Rsp,
                            Arg32::Signed(8 * round_up_even(env.size)),
                        )),
                    ]),
                    _ => unreachable!(),
                };
                is
            }
            Prim::Add
            | Prim::Sub
            | Prim::Mul
            | Prim::And
            | Prim::Or
            | Prim::Lt
            | Prim::Gt
            | Prim::Le
            | Prim::Ge
            | Prim::Eq
            | Prim::Neq => {
                let imm1 = &imms[0];
                let imm2 = &imms[1];
                let arg1 = compile_imm(imm1, &env);
                let arg2 = compile_imm(imm2, &env);
                let mut is = vec![
                    Instr::Mov(MovArgs::ToReg(Reg::Rax, arg1)),
                    Instr::Mov(MovArgs::ToReg(SCRATCH, arg2)),
                ];
                let args = BinArgs::ToReg(Reg::Rax, Arg32::Reg(SCRATCH));
                match op {
                    Prim::Add => {
                        
                       
                        let arg1 = compile_imm(&imms[0], &env);
                        let arg2 = compile_imm(&imms[1], &env);

                        //NUM + NUM CASE
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, arg1)));
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R8, arg2)));

                        //And together R9 and R8 contents and check last bit
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, arg1)));
                        is.push(Instr::Or(BinArgs::ToReg(Reg::R9, Arg32::Reg(Reg::R8))));

                        //check the lsb in both, if theres a 1 we know that we're not adding two nums together
                        is.push(Instr::Test(BinArgs::ToReg(Reg::R9, Arg32::Signed(1))));
                        is.push(Instr::Jnz((String::from(format!("ADD_CHECKCASES#{}", tag)))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Reg(Reg::R8))));
                        is.push(Instr::Jo(String::from("overflow_err")));
                        is.push(Instr::Jmp(String::from(format!("ADD_DONE#{}", tag))));


                        is.push(Instr::Label(String::from(format!("ADD_CHECKCASES#{}", tag))));

                        //CHECKING THE VALUE HELD IN RAX
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, Arg64::Reg(Reg::Rax))));
                        is.push(Instr::And(BinArgs::ToReg(Reg::R9, Arg32::Signed(0b111))));

                        //string case
                        is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Signed(0b011))));
                        is.push(Instr::Je( String::from(format!("ADD_LSTR#{}", tag)) ));

                        //char case
                        is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Signed(0b101))));
                        is.push(Instr::Je( String::from(format!("ADD_LCHAR#{}", tag)) ));

                        //if it isnt a string or a char it's an arith error
                        is.push(Instr::Jmp(String::from("arith_err")));


                        //LEFT SIDE IS A STRING CASE
                        is.push(Instr::Label(String::from(format!("ADD_LSTR#{}", tag))));
                        //CHECKING THE VALUE HELD IN R8
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, Arg64::Reg(Reg::R8))));
                        is.push(Instr::And(BinArgs::ToReg(Reg::R9, Arg32::Signed(0b111))));

                        //string case
                        is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Signed(0b011))));
                        is.push(Instr::Je( String::from(format!("ADD_STR_STR#{}", tag)) ));

                        //char case
                        is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Signed(0b101))));
                        is.push(Instr::Je( String::from(format!("ADD_STR_CHAR#{}", tag)) ));

                        //if it isnt a string or a char it's an arith error
                        is.push(Instr::Jmp(String::from("arith_err")));
                        

                        //STR + STR
                        is.push(Instr::Label(String::from(format!("ADD_STR_STR#{}", tag))));

                        //detag both RAX and R8
                        is.push(Instr::Sub(BinArgs::ToReg(Reg::Rax, Arg32::Signed(3))));
                        is.push(Instr::Sub(BinArgs::ToReg(Reg::R8, Arg32::Signed(3))));

                        //get their sizes and store them to the stack
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }))));
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R10, Arg64::Mem(MemRef { reg: Reg::R8, offset: 0 }))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R9, Arg32::Reg(Reg::R10))));

                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 0 }, Reg32::Reg(Reg::R9))));

                        //save RAX memory location to R8
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R8, Arg64::Reg(Reg::Rax))));

                        //overwrite R15 onto RAX and tag it as a string
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::R15))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Signed(3))));
                        
                        //get R8 length
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Mem(MemRef { reg: Reg::R8, offset: 0 }))));
                        //increment R8, R15
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Signed(8))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed(8))));

                        //loop through R8 and push it onto the heap

                        //loop
                        is.push(Instr::Label(String::from(format!("ADD_SS_1_LOOP#{}",tag))));
                        is.push(Instr::Cmp(BinArgs::ToReg(Reg::Rdi, (Arg32::Signed(0)))));
                        is.push(Instr::Jle( String::from(format!("ADD_SS_1_DONE#{}", tag)) ));

                        //pull memory at R8 into RSI
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Mem(MemRef { reg: Reg::R8, offset: 0 }))));
                        //write RSI to memory at R15
                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 0 }, Reg32::Reg(Reg::Rsi))));
                        //increment R8, R15, decrement RDI
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed(8))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Signed(8))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::Rdi, Arg32::Signed(-1))));

                        is.push(Instr::Jmp( String::from(format!("ADD_SS_1_LOOP#{}",tag)) ));

                        //done looping through string
                        is.push(Instr::Label(String::from(format!("ADD_SS_1_DONE#{}",tag))));


                        //pull original imms[1] into R8
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R8, arg2)));
                        is.push(Instr::Sub(BinArgs::ToReg(Reg::R8, Arg32::Signed(3))));


                        //get R8 length
                        is.push(Instr::Mov(MovArgs::ToReg((Reg::Rdi), Arg64::Mem(MemRef { reg: Reg::R8, offset: 0 }))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Signed(8))));

                        //loop through R8 and push it onto the heap

                        //loop
                        is.push(Instr::Label(String::from(format!("ADD_SS_2_LOOP#{}",tag))));
                        is.push(Instr::Cmp(BinArgs::ToReg(Reg::Rdi, (Arg32::Signed(0)))));
                        is.push(Instr::Jle( String::from(format!("ADD_DONE#{}", tag)) ));

                        //pull memory at R8 into RSI
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Mem(MemRef { reg: Reg::R8, offset: 0 }))));
                        //write RSI to memory at R15
                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 0 }, Reg32::Reg(Reg::Rsi))));
                        //increment R8, R15, decrement RDI
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed(8))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Signed(8))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::Rdi, Arg32::Signed(-1))));

                        is.push(Instr::Jmp( String::from(format!("ADD_SS_2_LOOP#{}",tag)) ));


                        //done
                        is.push(Instr::Jmp( String::from(format!("ADD_DONE#{}",tag))));


                        //STR + CHAR
                        is.push(Instr::Label(String::from(format!("ADD_STR_CHAR#{}", tag))));

                        //detag RAX and get the size
                        is.push(Instr::Sub(BinArgs::ToReg(Reg::Rax, (Arg32::Signed(3)))));
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Mem(MemRef { reg: Reg::Rax, offset: 0 }))));

                        //move a copy of Rdi into R9 and increment it
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, Arg64::Reg(Reg::Rdi))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R9, Arg32::Signed(1))));

                        //push this value onto the heap
                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: (0) }, Reg32::Reg(Reg::R9))));

                        //save memory address of RAX into R10
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R10, Arg64::Reg(Reg::Rax))));
                        //save R15 into RAX and tag it
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::R15))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Signed(0b011))));

                        //appending the string
                        //move R15 to the next available spot
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed(8))));
                        //move R10 to index 0
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Signed(8))));
                        //set RDI equal to the length of the string
                        is.push(Instr::Sub(BinArgs::ToReg(Reg::Rdi, Arg32::Signed(1))));

                        //loop
                        is.push(Instr::Label(String::from(format!("ADD_SC_LOOP#{}",tag))));
                        is.push(Instr::Cmp(BinArgs::ToReg(Reg::Rdi, (Arg32::Signed(0)))));
                        is.push(Instr::Jl( String::from(format!("ADD_SC_DONE#{}", tag)) ));

                        //pull memory at R10 into RSI
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Mem(MemRef { reg: Reg::R10, offset: 0 }))));
                        //write RSI to memory at R15
                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 0 }, Reg32::Reg(Reg::Rsi))));
                        //increment R10, R15, decrement RDI
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed(8))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R10, Arg32::Signed(8))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::Rdi, Arg32::Signed(-1))));

                        is.push(Instr::Jmp( String::from(format!("ADD_SC_LOOP#{}",tag)) ));

                        //done looping through string
                        is.push(Instr::Label(String::from(format!("ADD_SC_DONE#{}",tag))));
                        //push char in R8 onto the heap, increment R15
                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 0 }, Reg32::Reg(Reg::R8))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed(8))));
                        

                        is.push(Instr::Jmp( String::from(format!("ADD_DONE#{}",tag))));





                        //LEFT SIDE IS A CHAR CASE
                        is.push(Instr::Label(String::from(format!("ADD_LCHAR#{}", tag))));
                        //CHECKING THE VALUE HELD IN R8
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, Arg64::Reg(Reg::R8))));
                        is.push(Instr::And(BinArgs::ToReg(Reg::R9, Arg32::Signed(0b111))));

                        //string case
                        is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Signed(0b011))));
                        is.push(Instr::Je( String::from(format!("ADD_CHAR_STR#{}", tag)) ));

                        //char case
                        is.push(Instr::Cmp(BinArgs::ToReg(Reg::R9, Arg32::Signed(0b101))));
                        is.push(Instr::Je( String::from(format!("ADD_CHAR_CHAR#{}", tag)) ));

                        //if it isnt a string or a char it's an arith error
                        is.push(Instr::Jmp(String::from("arith_err")));


                        //CHAR + STR
                        is.push(Instr::Label(String::from(format!("ADD_CHAR_STR#{}", tag))));

                        //detag R8 and get the size
                        is.push(Instr::Sub(BinArgs::ToReg(Reg::R8, (Arg32::Signed(3)))));
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Mem(MemRef { reg: Reg::R8, offset: 0 }))));

                        //move a copy of R8 into R9 and increment it
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::R9, Arg64::Reg(Reg::Rdi))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R9, Arg32::Signed(1))));

                        //push this value onto the heap
                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: (0) }, Reg32::Reg(Reg::R9))));
                        //push the char onto the heap
                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: (8) }, Reg32::Reg(Reg::Rax))));
                        //save R15 into RAX and tag it
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::R15))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Signed(0b011))));

                        //appending the string
                        //move R15 to the next available spot
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed(16))));
                        //move R8 to index 0
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Signed(8))));
                        //set RDI equal to the length of the string
                        is.push(Instr::Sub(BinArgs::ToReg(Reg::Rdi, Arg32::Signed(1))));

                        //loop
                        is.push(Instr::Label(String::from(format!("ADD_CS_LOOP#{}",tag))));
                        is.push(Instr::Cmp(BinArgs::ToReg(Reg::Rdi, (Arg32::Signed(0)))));
                        is.push(Instr::Jl( String::from(format!("ADD_DONE#{}", tag)) ));

                        //pull memory at R8 into RSI
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rsi, Arg64::Mem(MemRef { reg: Reg::R8, offset: 0 }))));
                        //write RSI to memory at R15
                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 0 }, Reg32::Reg(Reg::Rsi))));
                        //increment R8, R15, decrement RDI
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed(8))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R8, Arg32::Signed(8))));
                        is.push(Instr::Add(BinArgs::ToReg(Reg::Rdi, Arg32::Signed(-1))));

                        is.push(Instr::Jmp( String::from(format!("ADD_CS_LOOP#{}",tag)) ));



                        //CHAR + CHAR
                        is.push(Instr::Label(String::from(format!("ADD_CHAR_CHAR#{}", tag))));

                        //making a new string size:2[rax,r8] on the heap
                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 0 }, Reg32::Imm(2))));
                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 8 }, Reg32::Reg(Reg::Rax))));
                        is.push(Instr::Mov(MovArgs::ToMem(MemRef { reg: Reg::R15, offset: 16 }, Reg32::Reg(Reg::R8))));

                        //saving ptr to memory into RAX
                        is.push(Instr::Mov(MovArgs::ToReg(Reg::Rax, Arg64::Reg(Reg::R15))));
                        //tagging RAX
                        is.push(Instr::Add(BinArgs::ToReg(Reg::Rax, Arg32::Signed(0b011))));

                        //incrementing R15
                        is.push(Instr::Add(BinArgs::ToReg(Reg::R15, Arg32::Signed(24))));

                        //jump to add done
                        is.push(Instr::Jmp(String::from(format!("ADD_DONE#{}", tag))));
                        //END CHAR + CHAR ********************************************************************


                        is.push(Instr::Label(String::from(format!("ADD_DONE#{}", tag))));
                        //returning is below
                    }
                    Prim::Sub => {
                        is.extend(prim2_num_checks(arg1, arg2, "arith_err"));
                        is.push(Instr::Sub(args));
                        is.push(Instr::Jo(String::from("overflow_err")));
                    }
                    Prim::Mul => {
                        is.extend(prim2_num_checks(arg1, arg2, "arith_err"));
                        is.extend(vec![
                            Instr::IMul(args),
                            Instr::Jo(String::from("overflow_err")),
                            Instr::Sar(BinArgs::ToReg(Reg::Rax, Arg32::Signed(1))),
                        ]);
                    }
                    Prim::And => {
                        is.extend(prim2_bool_checks(arg1, arg2, "log_err"));
                        is.extend(vec![Instr::And(args)])
                    }
                    Prim::Or => {
                        is.extend(prim2_bool_checks(arg1, arg2, "log_err"));
                        is.extend(vec![Instr::Or(args)])
                    }
                    Prim::Lt => {
                        is.extend(prim2_num_checks(arg1, arg2, "cmp_err"));
                        is.extend(cmp_args(args, Instr::Jl, *tag));
                    }
                    Prim::Le => {
                        is.extend(prim2_num_checks(arg1, arg2, "cmp_err"));
                        is.extend(cmp_args(args, Instr::Jle, *tag));
                    }
                    Prim::Gt => {
                        is.extend(prim2_num_checks(arg1, arg2, "cmp_err"));
                        is.extend(cmp_args(args, Instr::Jg, *tag));
                    }
                    Prim::Ge => {
                        is.extend(prim2_num_checks(arg1, arg2, "cmp_err"));
                        is.extend(cmp_args(args, Instr::Jge, *tag));
                    }
                    Prim::Neq => {
                        is.extend(cmp_args(args, Instr::Jne, *tag));
                    }
                    Prim::Eq => {
                        is.extend(cmp_args(args, Instr::Je, *tag));
                    }
                    _ => unreachable!(),
                }
                is
            }
        },
        SeqExp::Let {
            var,
            bound_exp,
            body,
            ..
        } => {
            let mut is = compile_with_env(bound_exp, env.clone());
            let var_offset = env.push(var);
            is.push(Instr::Mov(MovArgs::ToMem(
                MemRef {
                    reg: Reg::Rsp,
                    offset: var_offset,
                },
                Reg32::Reg(Reg::Rax),
            )));
            is.extend(compile_with_env(body, env));
            is
        }
        SeqExp::If {
            cond,
            thn,
            els,
            ann: tag,
        } => {
            let arg = compile_imm(cond, &env);
            let thn_is = compile_with_env(thn, env.clone());
            let els_is = compile_with_env(els, env);

            let else_lab = format!("else#{}", tag);
            let done_lab = format!("done#{}", tag);

            let mut is = vec![];

            is.extend(bool_test(arg, "if_err"));

            is.extend(vec![
                Instr::Mov(MovArgs::ToReg(Reg::Rax, arg)), // mov the condition to rax
                Instr::Mov(MovArgs::ToReg(SCRATCH, Arg64::Unsigned(SNAKE_TRUE))),
                Instr::Cmp(BinArgs::ToReg(Reg::Rax, Arg32::Reg(SCRATCH))), // test cond == TRUE
                Instr::Jne(else_lab.clone()), // jump to else if the cond is 0
                                              // otherwise we're in the tru branch
            ]);
            is.extend(thn_is); // then branch
            is.push(Instr::Jmp(done_lab.clone())); // skip over the else branch
            is.push(Instr::Label(else_lab)); // else branch
            is.extend(els_is);
            is.push(Instr::Label(done_lab));
            is
        }

        SeqExp::FunDefs { decls, body, ann } => {
            let mut is = Vec::new();
            for decl in decls {
                env.push_fun(&decl.name);
            }
            is.extend(compile_with_env(body, env.clone()));
            let pass_label = format!("skip_local_fundefs#{}", ann);
            is.push(Instr::Jmp(pass_label.clone()));
            for decl in decls {
                let mut local_env = env.clone();
                is.push(Instr::Label(user_fun_to_label(&decl.name)));
                for param in &decl.parameters {
                    local_env.push(param);
                }
                is.extend(compile_with_env(&decl.body, local_env));
                is.push(Instr::Ret);
            }
            is.push(Instr::Label(pass_label));
            is
        }

        SeqExp::InternalTailCall(f, args, _) => {
            let base_arg_offset = env.lookup_fun_offset(f).expect("foo");
            let mut is = Vec::new();
            // An internal tail call
            for (i, arg) in args.iter().enumerate() {
                is.extend(mov_to_mem64(
                    MemRef {
                        reg: Reg::Rsp,
                        offset: -8 * (1 + base_arg_offset + i32::try_from(i).unwrap()),
                    },
                    compile_imm(arg, &env),
                ))
            }
            is.push(Instr::Jmp(user_fun_to_label(f)));
            is
        }

        SeqExp::ExternalCall {
            fun_name: f,
            args,
            is_tail,
            ..
        } => {
            let mut is = Vec::new();
            if *is_tail {
                // Assumption: we made temporaries for all of these so
                // if we mov them in order it's no problem
                for (i, arg) in args.iter().enumerate() {
                    is.extend(mov_to_mem64(
                        MemRef {
                            reg: Reg::Rsp,
                            offset: -8 * (1 + i32::try_from(i).unwrap()),
                        },
                        compile_imm(arg, &env),
                    ))
                }
                is.push(Instr::Jmp(user_fun_to_label(f)));
                is
            } else {
                // say l is the num locals
                // rsp is the base.
                // last local is at [RSP - 8 * l]
                //
                // 0th arg is at
                // mov [rax - (8 * (l + 2 + 0))]
                let l = round_up_odd(env.size);

                is.push(Instr::Comment(format!("num locals: {}", l)));
                for (i, arg) in args.iter().enumerate() {
                    is.extend(mov_to_mem64(
                        MemRef {
                            reg: Reg::Rsp,
                            offset: -8 * (l + 2 + i32::try_from(i).unwrap()),
                        },
                        compile_imm(arg, &env),
                    ));
                }
                is.push(Instr::Sub(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(8 * l))));
                is.push(Instr::Call(user_fun_to_label(f)));
                is.push(Instr::Add(BinArgs::ToReg(Reg::Rsp, Arg32::Signed(8 * l))));
                is
            }
        }
    }
}

fn compile_fun(name: &str, params: &[String], body: &SeqExp<u32>) -> Vec<Instr> {
    let mut is = vec![Instr::Label(String::from(name))];
    let mut locals = CodeGenEnv::new(vec![]);
    for param_name in params {
        locals.push(param_name);
    }
    is.extend(compile_with_env(body, locals));
    is.push(Instr::Ret);
    is
}

pub fn compile_to_string<Span>(p: &SurfProg<Span>) -> Result<String, CompileErr<Span>>
where
    Span: Clone,
{   
    //uncomment the println! below to get the input code being compiled
    //println!("{}", p);
    
    // first check for errors
    check_prog(p)?;
    // then give all the variables unique names
    let uniq_p = uniquify(&tag_exp(p));
    // lift definitions to the top level
    // eprintln!("uniq'd:\n{:#?}", uniq_p);
    let (defs, main) = lambda_lift(&uniq_p);
    // eprintln!("lifted: {:#?}\n{:#?}", defs, main);
    let (t_defs, t_main) = tag_prog(&defs, &main);
    // then sequentialize
    let seq_p = tag_sprog(&seq_prog(&t_defs, &t_main));
    // eprintln!("sequential prog:\n{:#?}", seq_p);
    // then codegen
    let preamble = "\
         section .data
         HEAP:    times 1024 dq 0
         section .text
         global start_here
         extern print_snake_val
         extern snake_error
         extern ischar
         extern isstring
         extern isarray
";
    let epilogue = "\
start_here:
         push r15
         sub rsp, 8
         lea r15, [rel HEAP]
         call main
         add rsp, 8
         pop r15
         ret

overflow_err:
        mov rdi, 0
        call snake_error
arith_err:
        mov rdi, 1
        call snake_error
cmp_err:
        mov rdi, 2
        call snake_error
log_err:
        mov rdi, 3
        call snake_error
if_err:
        mov rdi, 4
        call snake_error
range_dec_err:
        mov rdi, 5
        call snake_error
index_oob_err:
        mov rdi, 6
        call snake_error
index_non_both:
        mov rdi, 7
        call snake_error
set_non_char:
        mov rdi, 8
        call snake_error
index_nan:
        mov rdi, 9
        call snake_error
length_not_array_error:
        mov rdi, 10
        call snake_error
 ";

    let mut buf = String::from(preamble);

    for d in seq_p.funs.iter() {
        buf.push_str(&instrs_to_string(&compile_fun(
            &user_fun_to_label(&d.name),
            &d.parameters,
            &d.body,
        )))
    }

    buf.push_str(&instrs_to_string(&compile_fun("main", &[], &seq_p.main)));
    buf.push_str(epilogue);

    Ok(buf)
}
