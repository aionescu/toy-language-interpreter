package com.aionescu.tli.ast.type;

public final class TInt implements Type {
  public static final TInt t = new TInt();

  private TInt() { }

  @Override
  public boolean equals(Object rhs) {
    return rhs instanceof TInt;
  }

  @Override
  public String toString() {
    return "Int";
  }
}
