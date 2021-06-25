#!/usr/bin/env tli

-- Shorter, but doesn't close the file.
-- let path = read Str from open "FS/Path.txt" in

let f' = open "FS/Path.txt" in
let path = read Str from f' in
close f';

let f = open path in

let a = read Num from f in
print a;

let a = read Num from f in
print a;

let b = read { a: Num, b: Str } from f in
print b;

close f
