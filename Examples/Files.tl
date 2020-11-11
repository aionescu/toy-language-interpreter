let f = "a.txt";
open f;

let a: Int;
a: Int = read f;
print a;

a: Int = read f;
print a;

let b: { a: Int, b: Str };
b: { a: Int, b: Str } = read f;
print b;

close f;
