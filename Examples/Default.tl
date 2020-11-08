a : _ <- default Int;
print a;

b : _ <- default Str;
print b;

-- Functions are not defaultable
--f : _ <- default Int -> Int;
--print f;

rec : _ <- default { a: Int, b: Str, c: (Int, Bool, (Str, Str)) };
print rec
