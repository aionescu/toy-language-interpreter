a : _ <- { a <- 2, b <- False };
b : _ <- a;

print a = b;

b.b <- True;
print a = b;
print a < b;

a' : _ <- (0, 1, 2);
b' : _ <- (0, 1, 3);
print a' = b';
print b' >= a';

-- Doesn't typecheck: Can't compare functions
{-
f : _ <- \(x : Int). x + 1;
print f = f
-}
