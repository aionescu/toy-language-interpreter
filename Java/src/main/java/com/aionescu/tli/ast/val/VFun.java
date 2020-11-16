package com.aionescu.tli.ast.val;

import java.util.function.UnaryOperator;

import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.eval.PanicException;

public final class VFun extends Val {
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
    throw new PanicException();
  }

  @Override
  public Type type() {
    throw new PanicException();
  }
}
