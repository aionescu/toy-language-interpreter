package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.type.Type;

public final class Int implements Val {
  public final int val;

  public Int(int val) {
    this.val = val;
  }

  @Override
  public Type type() {
    return Type.INT;
  }

  @Override
  public String toString() {
    return String.valueOf(val);
  }
}
