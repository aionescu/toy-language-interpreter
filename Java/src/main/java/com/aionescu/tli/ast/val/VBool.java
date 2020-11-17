package com.aionescu.tli.ast.val;

import java.util.function.UnaryOperator;

import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.eval.PanicException;
import com.aionescu.tli.utils.collections.set.Set;

public final class VBool extends Val {
  public final boolean val;

  public VBool(boolean val) {
    this.val = val;
  }

  @Override
  public String toString() {
    return val ? "True" : "False";
  }

  @Override
  public int compareTo(Val rhs) {
    if (!(rhs instanceof VBool))
      throw new PanicException();

    return Boolean.compare(val, ((VBool) rhs).val);
  }

  @Override
  public Type type() {
    return TBool.t;
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
