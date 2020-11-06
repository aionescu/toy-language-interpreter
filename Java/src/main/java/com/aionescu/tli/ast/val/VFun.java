package com.aionescu.tli.ast.val;

import java.util.function.UnaryOperator;

import com.aionescu.tli.exn.eval.InvalidComparisonException;

public final class VFun implements Val {
  public final UnaryOperator<Val> f;

  public VFun(UnaryOperator<Val> f) {
    this.f = f;
  }

  @Override
  public String toString() {
    return "<λ>";
  }

  @Override
  public int compareTo(Val arg0) {
    throw new InvalidComparisonException();
  }
}
