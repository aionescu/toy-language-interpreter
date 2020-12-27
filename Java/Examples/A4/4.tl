let v: &Int;
v = new 20;

let a: &&Int;
a = new v;

v = new 30;

print !!a;
