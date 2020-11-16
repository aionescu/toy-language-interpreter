package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.val.VStr;
import com.aionescu.tli.ast.val.Val;

public final class TStr implements Type {
  public static final TStr t = new TStr();

  private TStr() { }

  @Override
  public boolean equals(Object rhs) {
    return rhs instanceof TStr;
  }

  @Override
  public String toString() {
    return "Str";
  }

  @Override
  public boolean isOpaque() {
    return true;
  }

  @Override
  public Val defaultValue() {
    return new VStr("");
  }
}
