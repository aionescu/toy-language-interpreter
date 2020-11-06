package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.typeck.TypeMismatchException;

public final class Bool implements Val {
  public final boolean val;

  public Bool(boolean val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return val ? "True" : "False";
  }

  @Override
  public int compareTo(Val rhs) {
    if (rhs instanceof Bool)
      return Boolean.compare(val, ((Bool)rhs).val);
    else
      throw new TypeMismatchException(type(), rhs.type());
  }

  @Override
  public Type type() {
    return TBool.t;
  }
}
