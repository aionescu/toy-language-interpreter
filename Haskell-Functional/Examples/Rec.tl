let id = (n: Int) -> n in

let fac = new id in
fac := (n: Int) -> if n == 0 then 1 else n * !fac (n - 1);

print !fac 5;
print !fac (!fac 5);

print "Fibs";

let fib = new id in
fib := (n: Int) ->
  if n < 2
  then n
  else !fib (n - 1) + !fib (n - 2);

let printFibs = new ((a: Int) (b: Int) -> ()) in
printFibs := (crr: Int) (max: Int) ->
  if crr == max
  then ()
  else (print !fib crr; !printFibs (crr + 1) max);

!printFibs 0 30
