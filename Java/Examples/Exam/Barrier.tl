let v1: &Int;
let v2: &Int;
let v3: &Int;
let cnt: Int;

v1 = new 2;
v2 = new 3;
v3 = new 4;
cnt = barrier !v2;

fork {
  await cnt;
  v1 := !v1 * 10;
  print !v1
};

fork {
  await cnt;
  v2 := !v2 * 10;
  v2 := !v2 * 10;
  print !v2
};

await cnt;
print !v3;
