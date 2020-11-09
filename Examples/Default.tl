let a = default Int;
print a;

let b = default Str;
print b;

-- Functions are not defaultable
--let f = default Int -> Int;
--print f;

let rec = default { a: Int, b: Str, c: (Int, Bool, (Str, Str)) };
print rec;
