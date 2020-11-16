package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.val.VBool;
import com.aionescu.tli.ast.val.Val;

public final class TBool implements Type {
  public static final TBool t = new TBool();

  private TBool() { }

  @Override
  public boolean equals(Object rhs) {
    return rhs instanceof TBool;
  }

  @Override
  public String toString() {
    return "Bool";
  }

  @Override
  public boolean isOpaque() {
    return false;
  }

  @Override
  public Val defaultValue() {
    return new VBool(false);
  }
}
