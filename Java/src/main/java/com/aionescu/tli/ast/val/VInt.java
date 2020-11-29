package com.aionescu.tli.ast.val;

import java.math.BigInteger;
import java.util.function.UnaryOperator;

import com.aionescu.tli.ast.type.TInt;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.eval.PanicException;
import com.aionescu.tli.utils.data.set.Set;

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
      throw new PanicException();

    return val.compareTo(((VInt)rhs).val);
  }

  @Override
  public Type type() {
    return TInt.t;
  }

  @Override
  public Set<Integer> getInnerAddrs() {
    return Set.empty();
  }

  @Override
  public Val mapInnerAddrs(UnaryOperator<Integer> f) {
    return this;
  }
}
