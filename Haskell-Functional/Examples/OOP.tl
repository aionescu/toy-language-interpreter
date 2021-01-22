-- Closures are a poor man's objects

let mkPoint = (x: Int) (y: Int) ->
  let x' = new x in
  let y' = new y in

  { getX = (a: ()) -> !x'
  , setX = (x: Int) -> x' := x
  , getY = (a: ()) -> !y'
  , setY = (y: Int) -> y' := y
  }
in

let p = mkPoint 2 3 in

print p.getX ();
print p.getY ();

p.setX 7;
p.setY 8;

print p.getX ();
print p.getY ()
