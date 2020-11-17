let v: &Int;
v = new 20;

let a: &&Int;
a = new v;

print v;
-- print a;

print !!a;
