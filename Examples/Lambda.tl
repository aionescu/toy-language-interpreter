-- Inferred: f: num -> num
let f = (a: num) -> a + 2;
print f 2;
-- print f; -- Cannot print functions

let f': bool -> bool -> bool;
f' = (a : bool) (b : bool) -> a and b;

print f' True False or True;

-- Poor man's objects
let obj: { getX: () -> num, getY: () -> num };
obj = { getX = (a: ()) -> 2, getY = (a: ()) -> 3 };

print obj.getX ();
print obj.getY ();

-- Inferred: addPoints: { x: num, y: num } -> { x: num, y: num } -> { x: num, y: num };
let addPoints =
  (p : { x : num, y : num }) (p2 : { x : num, y : num }) ->
    { x = p.x + p2.x, y = p.y + p2.y };

print addPoints { x = 1, y = 2 } { x = 3, y = 2 };

-- Scope magic
let f'': num -> num;

if True {- Imagine complex condition here -} {
  let a = 2;
  f'' = (x: num) -> x + a;
  -- a goes out of scope here, but f'' still has access to it,
  -- because lambdas capture their current scope
} else {
  f'' = (x : num) -> x - 2;
};

print f 2;

-- Higher order function
let run: (() -> num) -> num;
run = (r: () -> num) -> r ();

print run (a: ()) -> 10;
