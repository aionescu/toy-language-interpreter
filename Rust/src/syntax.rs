use std::collections::BTreeMap;

#[derive(Debug)]
pub enum Type {
  Int,
  Bool,
  Str,
  Ref(Box<Type>),
  Tuple(Vec<Type>),
  Record(BTreeMap<String, Type>),
  Fun(Box<Type>, Box<Type>),
}

#[derive(Debug)]
pub enum ArithOp { Add, Sub, Mul, Div, Rem }

#[derive(Debug)]
pub enum LogicOp { And, Or }

#[derive(Debug)]
pub enum CompOp { Gt, Gte, Lt, Lte, Eq, Neq }

#[derive(Debug)]
pub enum Expr {
  Int(i32),
  Bool(bool),
  Str(String),
  Var(String),
  Arith(Box<Expr>, ArithOp, Box<Expr>),
  Logic(Box<Expr>, LogicOp, Box<Expr>),
  Comp(Box<Expr>, CompOp, Box<Expr>),
  Deref(Box<Expr>),
  Default(Type),
  Tuple(Vec<Expr>),
  Record(BTreeMap<String, Expr>),
  TupleMember(Box<Expr>, i32),
  RecordMember(Box<Expr>, String),
  TupleWith(Box<Expr>, BTreeMap<i32, Expr>),
  RecordWith(Box<Expr>, BTreeMap<String, Expr>),
  Union(Box<Expr>, Box<Expr>),
  Lam(String, Type, Box<Expr>),
  App(Box<Expr>, Box<Expr>),
}

impl From<i32> for Expr {
  fn from(v: i32) -> Expr {
    return Expr::Int(v);
  }
}

impl From<bool> for Expr {
  fn from(v: bool) -> Expr {
    return Expr::Bool(v);
  }
}

impl From<&str> for Expr {
  fn from(v: &str) -> Expr {
    return Expr::Str(v.to_string());
  }
}

#[derive(Debug)]
pub enum Stmt {
  Nop,
  Print(Expr),
  Decl(String, Type),
  DeclAssign(String, Option<Type>, Expr),
  Assign(String, Expr),
  If(Expr, Box<Stmt>, Box<Stmt>),
  Compound(Box<Stmt>, Box<Stmt>),
  While(Expr, Box<Stmt>),
  New(String, Expr),
  WriteAt(Expr, Expr),
  Fork(Box<Stmt>),
  Open(Expr),
  Close(Expr),
  Read(String, Expr),
}
