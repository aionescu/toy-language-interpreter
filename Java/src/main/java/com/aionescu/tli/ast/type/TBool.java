package com.aionescu.tli.ast.type;

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
}
