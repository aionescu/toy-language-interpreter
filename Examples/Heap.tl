let r: &Int;
r = new 2;

let i = 2;
while i < 10 {
  r = new i;
  i = i + 1;
};

r := !r + 2;
print !r;
