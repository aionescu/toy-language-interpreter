let f = "FS/Meta.txt" in
open f;

let f = read Str from f in

close "FS/Meta.txt";
open f;

let a = read Int from f in
print a;

let a = read Int from f in
print a;

let b = read { a: Int, b: Str } from f in
print b;

close f
