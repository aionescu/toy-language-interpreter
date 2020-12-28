let meta: File = open "FS/Path.txt" in

let path = read Str from meta in
close meta;

let f = open path in

let a = read Int from f in
print a;

let a = read Int from f in
print a;

let b = read { a: Int, b: Str } from f in
print b;

close f
