let f: File;
open f = "a.txt";

let a: Int;
read a: Int = f;
print a;

read a: Int = f;
print a;

let b: { a: Int, b: Str };
read b: { a: Int, b: Str } = f;
print b;

close f;
