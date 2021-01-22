package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.TypeIsOpaqueException;

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
  public boolean isOpaque() {
    return true;
  }

  @Override
  public Val defaultValue() {
    throw new TypeIsOpaqueException(this);
  }
}
