package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.TypeNotDefaultableException;

public final class TFun implements Type {
  public final Type in, out;

  public TFun(Type in, Type out) {
    this.in = in;
    this.out = out;
  }

  @Override
  public boolean equals(Object rhs) {
    if (!(rhs instanceof TFun))
      return false;

    var fun = (TFun)rhs;
    return in.equals(fun.in) && out.equals(fun.out);
  }

  @Override
  public String toString() {
    return String.format("(%s -> %s)", in, out);
  }

  @Override
  public boolean isComparable() {
    return false;
  }

  @Override
  public boolean isShowable() {
    return false;
  }

  @Override
  public boolean isDefaultable() {
    return false;
  }

  @Override
  public Val defaultValue() {
    throw new TypeNotDefaultableException(this);
  }
}
