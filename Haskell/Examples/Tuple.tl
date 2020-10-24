a : (Int, Int);
a <- (2, 2);
print a;

b : (Bool, (Int, Bool)) <- (False, (2, True));
print b;

c : (Int, Int) <- (0, 0);
c.0 <- 1;
print c;

d : (Int, Int);
d <- (c.0, 2);
-- Functional update
d <- { c | 1 <- 2 }; -- same as above statement
print d;

-- Empty tuple / Unit type
e : () <- ();
print e;

-- f : (Int) == f : Int
-- To make a 1-tuple, add a trailing comma
f : (Int,) <- (2,);
print f;

-- Nested assignment, no need for lenses ;)
g : ((((Int, Int), Int), Int), Int) <- ((((0, 0), 0), 0), 0);
g.0.0.0.0 <- 1;
print g;

-- You can also do it the long way
g <- { g | 0 <- { g.0 | 0 <- { g.0.0 | 0 <- { g.0.0.0 | 0 <- 1 } } } };
print g;

-- Multiple updates at once
h : _ <- (0, 0, 0, 0);
print { h | 1 <- 1, 2 <- 2, 3 <- 1 + 2 }
