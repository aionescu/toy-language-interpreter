let v: Int;
let a: &Int;

v = 10;
a = new 22;

fork {
  a := 30;
  v = 32;

  print v;
  print !a;
};

print v;
print !a;
