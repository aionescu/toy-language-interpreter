let path = read Str from open "FS/Path.txt" in

let f = open path in

let a = read Int from f in
print a;

let a = read Int from f in
print a;

let b = read { a: Int, b: Str } from f in
print b;

close f
