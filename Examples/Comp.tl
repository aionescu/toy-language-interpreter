let a = { a = 2, b = False };
b : _ = a;

print a == b;

b.b = True;
print a == b;
print a < b;

let a' = (0, 1, 2);
let b' = (0, 1, 3);
print a' == b';
print b' >= a';

-- Doesn't typecheck: Can't compare functions
{-
let f = (x: num) -> x + 1;
print f == f;
-}
