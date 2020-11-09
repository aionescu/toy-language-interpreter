let a = 2;
let b = 0;

-- Change to 'and' and get a DivByZero
if True or a / b == 1 {
  print 2;
};

-- Change to 'or' and get a DivByZero
if False and a / b == 1 { } else {
  print 3;
};
