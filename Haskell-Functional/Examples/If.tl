#!/home/oldpug/.cabal/bin/tli run

let not = (a: Bool) ->
  if a
  then False
  else True
in

let a = True in

if a
then print "a is True"
else print ();

if not a
then print "a is False"
else print ()
