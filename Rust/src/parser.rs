#![macro_use]

pub use std::collections::BTreeMap;
pub use crate::syntax::*;

macro_rules! map_i32 {
  ($(($k:expr, $v:expr)),*) => {
    {
      let mut map = BTreeMap::new();
      $(
        map.insert($k, $v);
      )*
      map
    }
  };
}

macro_rules! map_ident {
  ($(($k:ident, $v:expr)),*) => {
    {
      let mut map = BTreeMap::new();
      $(
        map.insert(stringify!($k).to_string(), $v);
      )*
      map
    }
  };
}

macro_rules! expr {
  ({ $($f:ident = $t:tt),* }) => { Expr::Record(map_ident![$(($f, expr!($t))),*]) };
  ({ $lhs:tt | $($f:literal = $t:tt),* }) => { Expr::TupleWith(Box::new(expr!($lhs)), map_i32![$(($f, expr!($t))),*]) };
  ({ $lhs:tt | $($f:ident = $t:tt),* }) => { Expr::RecordWith(Box::new(expr!($lhs)), map_ident![$(($f, expr!($t))),*]) };

  (()) => { Expr::Tuple(vec![]) };
  (($fst:tt , $($rest:tt),*)) => { Expr::Tuple(vec![expr!($fst), $(expr!($rest)),*]) };
  (($($t:tt)*)) => { expr!($($t)*) };

  (($i:ident : $($t:tt)*) -> $($e:tt)*) => { Expr::Lam(stringify!($i).to_string(), typ!($($t)*), Box::new(expr!($($e)*))) };

  ($t:tt . $f:literal) => { Expr::TupleMember(Box::new(expr!($t)), $f) };
  ($t:tt . $f:ident) => { Expr::RecordMember(Box::new(expr!($t)), stringify!($f).to_string()) };

  (default $($t:tt)*) => { Expr::Default(typ!($($t)*)) };
  (! $($t:tt)*) => { Expr::Deref(Box::new(expr!($($t)*))) };

  ($v:literal) => { Expr::from($v) };
  ($i:ident) => { Expr::Var(stringify!($i).to_string()) };

  ($f:tt $x:tt) => { Expr::App(Box::new(expr!($f)), Box::new(expr!($x))) };

  ($lhs:tt + $($rhs:tt)*) => { Expr::Arith(Box::new(expr!($lhs)), ArithOp::Add, Box::new(expr!($($rhs)*))) };
  ($lhs:tt - $($rhs:tt)*) => { Expr::Arith(Box::new(expr!($lhs)), ArithOp::Sub, Box::new(expr!($($rhs)*))) };
  ($lhs:tt * $($rhs:tt)*) => { Expr::Arith(Box::new(expr!($lhs)), ArithOp::Mul, Box::new(expr!($($rhs)*))) };
  ($lhs:tt / $($rhs:tt)*) => { Expr::Arith(Box::new(expr!($lhs)), ArithOp::Div, Box::new(expr!($($rhs)*))) };
  ($lhs:tt % $($rhs:tt)*) => { Expr::Arith(Box::new(expr!($lhs)), ArithOp::Rem, Box::new(expr!($($rhs)*))) };

  ($lhs:tt and $($rhs:tt)*) => { Expr::Logic(Box::new(expr!($lhs)), LogicOp::And, Box::new(expr!($($rhs)*))) };
  ($lhs:tt or $($rhs:tt)*) => { Expr::Logic(Box::new(expr!($lhs)), LogicOp::Or, Box::new(expr!($($rhs)*))) };

  ($lhs:tt & $($rhs:tt)*) => { Expr::Union(Box::new(expr!($lhs)), Box::new(expr!($($rhs)*))) };

  ($lhs:tt <= $($rhs:tt)*) => { Expr::Comp(Box::new(expr!($lhs)), CompOp::Lte, Box::new(expr!($($rhs)*))) };
  ($lhs:tt < $($rhs:tt)*) => { Expr::Comp(Box::new(expr!($lhs)), CompOp::Lt, Box::new(expr!($($rhs)*))) };
  ($lhs:tt >= $($rhs:tt)*) => { Expr::Comp(Box::new(expr!($lhs)), CompOp::Gte, Box::new(expr!($($rhs)*))) };
  ($lhs:tt > $($rhs:tt)*) => { Expr::Comp(Box::new(expr!($lhs)), CompOp::Gt, Box::new(expr!($($rhs)*))) };
  ($lhs:tt == $($rhs:tt)*) => { Expr::Comp(Box::new(expr!($lhs)), CompOp::Eq, Box::new(expr!($($rhs)*))) };
  ($lhs:tt != $($rhs:tt)*) => { Expr::Comp(Box::new(expr!($lhs)), CompOp::Neq, Box::new(expr!($($rhs)*))) };
}

macro_rules! typ {
  ({ $($f:ident : $t:tt),* }) => { Type::Record(map_ident![$(($f, typ!($t))),*]) };

  (($fst:tt , $($rest:tt),*)) => { Type::Tuple(vec![typ!($fst), $(typ!($rest)),*]) };
  (()) => { Type::Tuple(vec![]) };
  (($($t:tt)*)) => { typ!($($t)*) };

  ($i:tt -> $($o:tt)*) => { Type::Fun(Box::new(typ!($i)), Box::new(typ!($($o)*))) };

  (&& $($t:tt)*) => { typ!(& & $($t)*) };
  (& $($t:tt)*) => { Type::Ref(Box::new(typ!($($t)*))) };

  ($t:ident) => { Type::$t };
}

macro_rules! stmt {
  (print $($t:tt)*) => { Stmt::Print(expr!($($t)*)) };
  (open $($t:tt)*) => { Stmt::Open(expr!($($t)*)) };
  (close $($t:tt)*) => { Stmt::Close(expr!($($t)*)) };

  (let $i:ident : $t:tt = $($e:tt)*) => { Stmt::DeclAssign(stringify!($i).to_string(), Some(typ!($t)), expr!($($e)*)) };
  (let $i:ident : $($t:tt)*) => { Stmt::Decl(stringify!($i).to_string(), typ!($($t)*)) };
  (let $i:ident = $($e:tt)*) => { Stmt::DeclAssign(stringify!($i).to_string(), None, expr!($($e)*)) };

  ($i:ident = new $($t:tt)*) => { Stmt::New(stringify!($i).to_string(), expr!($($t)*)) };
  ($i:ident = read $($t:tt)*) => { Stmt::Read(stringify!($i).to_string(), expr!($($t)*)) };

  ($i:ident = $($t:tt)*) => { Stmt::Assign(stringify!($i).to_string(), expr!($($t)*)) };
  ($t:tt := $($e:tt)*) => { Stmt::WriteAt(expr!($t), expr!($($e)*)) };

  (if ($($cond:tt)*) {$($then:tt)*} else {$($else:tt)*}) => { Stmt::If(expr!($($cond)*), Box::new(stmt!($($then)*)), Box::new(stmt!($($else)*))) };
  (while ($($cond:tt)*) {$($body:tt)*}) => { Stmt::While(expr!($($cond)*), Box::new(stmt!($($body)*))) };
  (fork {$($body:tt)*}) => { Stmt::Fork(Box::new(stmt!($($body)*))) };

  ([$($stmt:tt)*] ; $($rest:tt)*) => { Stmt::Compound(Box::new(stmt!($($stmt)*)), Box::new(stmt!($($rest)*))) };
  () => { Stmt::Nop };
}

macro_rules! tl {
  ($($t:tt)*) => { stmt!($($t)*) };
}
