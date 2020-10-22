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
d <- c with 1 = 2; -- same as above statement
print d
