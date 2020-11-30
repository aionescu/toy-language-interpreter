#![allow(dead_code)]
#![allow(unused_macros)]
#![allow(unused_imports)]
#![allow(unused_mut)]

mod syntax;
mod parser;

use crate::parser::*;

fn main() {
  println!("{:?}", expr!(b));
  println!("{:?}", expr!(2));
  println!("{:?}", expr!((((((((((2)))))))))));
  println!("{:?}", expr!(true));
  println!("{:?}", expr!(false));
  println!("{:?}", expr!(a));
  println!("{:?}", expr!{12 + false + b - c});
  println!("{:?}", expr!(true or false));
  println!();

  println!("{:?}", stmt!());
  println!("{:?}", stmt!(print 2));
  println!("{:?}", stmt!(let i: Int));
  println!("{:?}", stmt!(let j: Bool));
  println!("{:?}", stmt!(j = a));
  println!("{:?}", stmt!(i = 2));
  println!("{:?}", stmt!(length = 2 + false));
  println!("{:?}", stmt!(if (true or false) { print false } else { print 2 }));
  println!("{:?}", stmt!([print b]; [print a]; print b));
  println!();

  let program = tl! {
    [let i: Int];
    [i = 2];
    [print i + 2];

    if (true and false) {
      // You can write chained operator calls,
      // but they associate to the right
      // (cause that's the only to do it with macros)
      print true and true or false + 2 - a / true * b % c
    } else {
      print false
    }
  };

  println!("{:?}", program);
  println!();

  let str_while = tl! {
    [let s: Str = "abc\n"];
    [let i = 2];
    while (i <= 10) {
      [print s + "\n"];
      i = i + 1
    }
  };

  println!("{:?}", str_while);
  println!();

  // The infamous "20 vs 30" example
  let heap = stmt! {
    [let v: &Int];
    [v = new 20];

    [let a: &&Int];
    [a = new v];
    [v = new 30];

    [print !!a];

    // Also testing WriteAt
    [v := 40];
    [a := a];
    [(!a) := 30];
  };

  println!("{:?}", heap);
  println!();

  let fork = tl! {
    [let a = 2];
    [fork { [print a]; print a }];
    print a + 2
  };

  println!("{:?}", fork);
  println!();

  let files_default = tl! {
    [open "a"];
    [let a = "a.txt"];
    [open a];
    [close a];
    [a = read a];
    [a = default Int];
    [a = default &Int];
  };

  println!("{:?}", files_default);
  println!();

  let rec_lam = tl! {
    [let a: { a: Int, b: Str }];
    [a = { a = 2, b = "a", c = (2 + 3), d = { a = 2, b = false } }];

    [a = { a | x = 2, c = false, d = { (2 + 3) | lam = ((a: Int) -> a + 2) } } ];

    [print a.b];
    [print (a.a).a];
    [print (a + b).c];

    [let f: Int -> Int];
    [f = (a: Int) -> a + 2];

    [let g: Int -> (&Int) -> Int];
    [g = (a: Int) -> (b: &Int) -> a + !b];

    [x = new f a];
    [(f x) := f a];

    [print f a];
    [print (f a) b];
    [print f (a b)];
    [print f { x = 2, y = 3 }];

    [print a & b];
    [x = a & 2];
    [y = { a = 2 } & { b = false }];
  };

  println!("{:?}", rec_lam);
  println!();

  let tuples = tl! {
    [let a: (Int, Bool, { a: Int, b: (Str -> &Int) })];
    [a = (2, 3)];

    // 1-tuple
    [let a: (Int,)];
    [a = (2,)];

    [let a = x.0];
    [let b = { x | 0 = 2, 2 = (2 + 4) }];

    // Unit types
    [let a: { } = { }];
    [let a: () = ()];
  };

  println!("{:?}", tuples);
  println!();
}
