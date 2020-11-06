package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.typeck.TypeMismatchException;

public final class VBool implements Val {
  public final boolean val;

  public VBool(boolean val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return val ? "True" : "False";
  }

  @Override
  public int compareTo(Val rhs) {
    if (!(rhs instanceof VBool))
      throw new TypeMismatchException(type(), rhs.type());

    return Boolean.compare(val, ((VBool)rhs).val);
  }

  @Override
  public Type type() {
    return TBool.t;
  }
}
