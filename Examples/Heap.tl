let r: &num;
r = new 2;

let i = 2;
while i < 1000 {
  r = new i;
  i = i + 1;
};

r := !r + 2;
print !r;
