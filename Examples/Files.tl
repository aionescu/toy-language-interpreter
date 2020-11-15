let f = "Meta.txt";
open f;

f: str = read f; -- o_O
-- This actually leaks "Meta.txt" if you have no other reference to its name

close "Meta.txt";

open f;

let a: num;
a: num = read f;
print a;

a: num = read f;
print a;

let b: { a: num, b: str };
b: { a: num, b: str } = read f;
print b;

close f;
