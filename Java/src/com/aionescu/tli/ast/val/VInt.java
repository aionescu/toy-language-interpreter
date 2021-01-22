package com.aionescu.tli.ast.val;

import java.math.BigInteger;

import com.aionescu.tli.ast.type.TInt;
import com.aionescu.tli.ast.type.Type;

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
    return rhs instanceof VInt ? val.compareTo(((VInt)rhs).val) : -1;
  }

  @Override
  public Type type() {
    return TInt.t;
  }
}
