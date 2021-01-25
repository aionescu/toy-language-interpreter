let v: Int;
let x: Int;
let y: Int;

v = 0;

repeat {
  fork {
    print v;
    v = v - 1
  };
  v = v + 1
} until v == 3;

x = 1;
;

y = 3;
;

print v * 10;
