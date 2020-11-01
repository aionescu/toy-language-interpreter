package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.type.Type;

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
  public Type type() {
    return Type.BOOL;
  }
}
