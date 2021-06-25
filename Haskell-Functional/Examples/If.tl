#!/usr/bin/env tli

let not (a: Bool) =
  if a
  then False
  else True
in

let a = True in

if a
then print "a is True"
else ();

if not a
then print "a is False"
else ()
