#!/home/oldpug/.cabal/bin/tli run

let a: Bool;
a = True;

if a and True or False {
  print a;
} else {
  print 2;
};

let b: Int;
b{-Nested {- multiline -}


  comment -} = 2;
print b;

-- Single line comment
let c: Int;
c = -1;
print c;

-- Declare-and-assign shorthand syntax
let d: Int = 1 + 2 * 3;
print d;

-- If with no else branch
if True {
  print {- Multiline comment -} False;
};

-- Declare-and-assign + Type inference
let e = 2;

if e == 2 {
  while e <= 10 {
    print e;
    e = e + 1;
  };
};
