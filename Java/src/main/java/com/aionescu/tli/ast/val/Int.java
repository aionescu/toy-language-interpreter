package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.type.TInt;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.typeck.TypeMismatchException;

public final class Int implements Val {
  public final int val;

  public Int(int val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return String.valueOf(val);
  }

  @Override
  public int compareTo(Val rhs) {
    if (rhs instanceof Int)
      return Integer.compare(val, ((Int)rhs).val);
    else
      throw new TypeMismatchException(type(), rhs.type());
  }

  @Override
  public Type type() {
    return TInt.t;
  }
}
