let v: Int;
let a: &Int;

v = 10;
a = new 22;

let i = 10;

while i >= 0 {
  fork {
    a := 30;
    v = 32;

    print v;
    print !a;
  };

  i = i - 1;
};

print v;
print !a;
