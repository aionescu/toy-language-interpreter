let a: (num, num);
a = (2, 2);
print a;

let b: (bool, (num, bool)) = (False, (2, True));
print b;

let c: (num, num) = (0, 0);
c.0 = 1;
print c;

let d: (num, num);
d = (c.0, 2);
-- Functional update
d = { c | 1 = 2 }; -- same as above statement
print d;

-- Empty tuple / Unit type
let e: () = ();
print e;

-- f: (num) == f: num
-- To make a 1-tuple, add a trailing comma
let f: (num,) = (2,);
print f;

-- Nested assignment
let g: ((((num, num), num), num), num) = ((((0, 0), 0), 0), 0);
g.0.0.0.0 = 1;
print g;

-- You can also do it the long way
g = { g | 0 = { g.0 | 0 = { g.0.0 | 0 = { g.0.0.0 | 0 = 1 } } } };
print g;

-- Multiple updates at once
let h = (0, 0, 0, 0);
print { h | 1 = 1, 2 = 2, 3 = 1 + 2 };
