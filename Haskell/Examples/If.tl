#!/home/oldpug/.cabal/bin/tli run

let not = (a: Bool) -> if a { False } { True } in

let a = True in

if a {
  print "a is True"
} {
  ()
};

if not a {
  print "a is False"
} {
  print ()
}
