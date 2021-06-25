#!/usr/bin/env tli

-- Closures are a poor man's objects

type Point =
  { getX: () -> Num
  , setX: Num -> ()
  , getY: () -> Num
  , setY: Num -> ()
  }
in

let point (x: Num) (y: Num) =
  let x' = ref x in
  let y' = ref y in

  { getX () = !x'
  , setX (x: Num) = x' := x
  , getY (a: ()) = !y'
  , setY (y: Num) = y' := y
  }
in

let add (a: Point) (b: Point) =
  point (a.getX () + b.getX ()) (a.getY () + b.getY ())
in

let showP (p: Point) = show (p.getX (), p.getY ()) in

let p = point 2 3 in

print showP p;

p.setX 7;
p.setY 8;

print showP p;

let q = point -1 7 in
print showP (add p q)
