a : { x : Int, y : Int };
a <- { x <- 2, y <- 3 };

-- Inferred: b : { a : Bool };
b : _ <- { a <- True };

-- Record union/merging
-- Inferred: c : { a : Bool, x : Int, y : Int }
c : _ <- a | b;
c.a <- False;
print c;

-- Mixed record/tuple member access
d : ({ a : ({ b : Bool }, Int) }, Int);
d <- ({ a <- ({ b <- False }, 1) }, 0);
d.0.a.0.b <- True;
print d.0.a.0.b;
print d;

-- Mixed record/tuple update
d <- d with { 0 <- d.0 with { a <- d.0.a with { 0 <- d.0.a.0 with { b <- False } } } };
print d;

-- Field order doesn't matter
-- Fields get sorted lexicographically
e : { a : Int, b : Bool } <- { a <- 0, b <- False };
f : { b : Bool, a : Int } <- e;
print e;
print f;

-- Actually, there are 2 Unit types :D (Empty record)
g : { } <- { };
print g;

print f;
f <- f with { }; -- Empty record update (works with tuples too)
print f
