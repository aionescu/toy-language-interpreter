let a: { x: num, y: num };
a = { x = 2, y = 3 };

-- Inferred: b: { a: bool }
let b = { a = True, };

-- Record union (left-biased)
-- Inferred: c: { a: bool, x: num, y: num }
let c = a & b;
c.a = False;
print c;

-- Mixed record/tuple member access
let d: ({ a: ({ b: bool }, num) }, num);
d = ({ a = ({ b = False }, 1) }, 0);
d.0.a.0.b = True;
print d.0.a.0.b;
print d;

-- Mixed record/tuple update
d = { d | 0 = { d.0 | a = { d.0.a | 0 = { d.0.a.0 | b = False } } } };
print d;

-- Field order doesn't matter
-- Fields get sorted lexicographically
let e: { a: num, b: bool } = { a = 0, b = False };
let f: { b: bool, a: num } = e;
print e;
print f;

-- Empty record / Union type
-- Distinct from empty tuple
let g: { } = { };
print g;

print f;
f = {f | }; -- Empty record update (works with tuples too)
print f;
