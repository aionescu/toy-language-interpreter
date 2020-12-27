let a: { x: Int, y: Int };
a = { x = 2, y = 3 };

-- Inferred: b: { a: Bool }
let b = { a = True, };

-- Record union (left-biased)
-- Inferred: c: { a: Bool, x: Int, y: Int }
let c = a & b;
c.a = False;
print c;

-- Mixed record/tuple member access
let d: ({ a: ({ b: Bool }, Int) }, Int);
d = ({ a = ({ b = False }, 1) }, 0);
d.0.a.0.b = True;
print d.0.a.0.b;
print d;

-- Mixed record/tuple update
d = { d | 0 = { d.0 | a = { d.0.a | 0 = { d.0.a.0 | b = False } } } };
print d;

-- Field order doesn't matter
-- Fields get sorted lexicographically
let e: { a: Int, b: Bool } = { a = 0, b = False };
let f: { b: Bool, a: Int } = e;
print e;
print f;

-- Empty record / Union type
-- Distinct from empty tuple
let g: { } = { };
print g;

print f;
f = {f | }; -- Empty record update (works with tuples too)
print f;
