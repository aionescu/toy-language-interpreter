#!/usr/bin/env tli

let a = { x = 2, y = 3 } in
let b = { a = True, } in

-- Record union (right-biased)
-- Inferred: c: { a: Bool, x: Num, y: Num }
let c = a & b in
print c;

-- Union can change type of fields
print c & { a = 2 };

-- Mixed record/tuple member access
let d: ({ a: ({ b: Bool }, Num) }, Num) = ({ a = ({ b = False }, 1) }, 0) in
print d.0.a.0.b;
print d;

-- Mixed record/tuple update
print c & { e = "Abc" };

-- Field order doesn't matter
-- Fields get sorted lexicographically
let e: { a: Num, b: Bool } = { a = 0, b = False } in
let f: { b: Bool, a: Num } = e in
print e;
print f;

-- Empty record
-- Distinct from empty tuple
let g: { } = { } in
print g
