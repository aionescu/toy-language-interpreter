#!/usr/bin/env tli

let r: &Num = ref 2 in
print !r;

r := 3;
print !r
