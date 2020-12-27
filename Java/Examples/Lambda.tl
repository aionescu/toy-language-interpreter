-- Inferred: f: Int -> Int
let f = (a: Int) -> a + 2;
print f 2;
-- print f; -- Cannot print functions

let f': Bool -> Bool -> Bool;
f' = (a : Bool) (b : Bool) -> a and b;

print f' True False or True;

-- Poor man's objects
let obj: { getX: () -> Int, getY: () -> Int };
obj = { getX = (a: ()) -> 2, getY = (a: ()) -> 3 };

print obj.getX ();
print obj.getY ();

-- Inferred: addPoints: { x: Int, y: Int } -> { x: Int, y: Int } -> { x: Int, y: Int };
let addPoints =
  (p : { x : Int, y : Int }) (p2 : { x : Int, y : Int }) ->
    { x = p.x + p2.x, y = p.y + p2.y };

print addPoints { x = 1, y = 2 } { x = 3, y = 2 };

-- Scope magic
let f'': Int -> Int;

if True {- Imagine complex condition here -} {
  let a = 2;
  f'' = (x: Int) -> x + a;
  -- a goes out of scope here, but f'' still has access to it,
  -- because lambdas capture their current scope
} else {
  f'' = (x : Int) -> x - 2;
};

print f 2;

-- Higher order function
let run: (() -> Int) -> Int;
run = (r: () -> Int) -> r ();

print run (a: ()) -> 10;
