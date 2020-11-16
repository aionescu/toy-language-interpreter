package com.aionescu.tli.ast.type;

import java.math.BigInteger;

import com.aionescu.tli.ast.val.VInt;
import com.aionescu.tli.ast.val.Val;

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

  @Override
  public boolean isOpaque() {
    return false;
  }

  @Override
  public Val defaultValue() {
    return new VInt(BigInteger.ZERO);
  }
}
