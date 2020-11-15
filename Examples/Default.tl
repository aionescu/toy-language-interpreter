let a = default num;
print a;

let b = default str;
print b;

-- Functions are not defaultable
--let f = default num -> num;
--print f;

let rec = default { a: num, b: str, c: (num, bool, (str, str)) };
print rec;
