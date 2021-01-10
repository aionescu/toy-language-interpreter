package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.Type;

public final class VBool extends Val {
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
    return rhs instanceof VBool ? Boolean.compare(val, ((VBool) rhs).val) : -1;
  }

  @Override
  public Type type() {
    return TBool.t;
  }
}
