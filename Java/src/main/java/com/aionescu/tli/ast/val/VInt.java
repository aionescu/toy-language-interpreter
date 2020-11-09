package com.aionescu.tli.ast.val;

import java.math.BigInteger;

import com.aionescu.tli.exn.eval.InvalidComparisonException;

public final class VInt extends Val {
  public final BigInteger val;

  public VInt(BigInteger val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return String.valueOf(val);
  }

  @Override
  public int compareTo(Val rhs) {
    if (!(rhs instanceof VInt))
      throw new InvalidComparisonException();

    return val.compareTo(((VInt)rhs).val);
  }
}
