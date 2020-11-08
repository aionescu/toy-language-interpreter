x : _ <- 100;

r : _ <- 1;
i : _ <- 1;

while i <= x {
  r <- i * r;
  i <- i + 1
};

print r
