package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.type.TInt;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.typeck.TypeMismatchException;

public final class VInt implements Val {
  public final int val;

  public VInt(int val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return String.valueOf(val);
  }

  @Override
  public int compareTo(Val rhs) {
    if (!(rhs instanceof VInt))
      throw new TypeMismatchException(type(), rhs.type());

    return Integer.compare(val, ((VInt)rhs).val);
  }

  @Override
  public Type type() {
    return TInt.t;
  }
}
