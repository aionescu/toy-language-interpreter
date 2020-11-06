package com.aionescu.tli.ast.val;

import com.aionescu.tli.exn.eval.InvalidComparisonException;

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
      throw new InvalidComparisonException();

    return Boolean.compare(val, ((VBool)rhs).val);
  }
}
