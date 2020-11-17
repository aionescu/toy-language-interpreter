package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.eval.PanicException;

public final class TRef implements Type {
  public final Type inner;

  public TRef(Type inner) {
    this.inner = inner;
  }

  @Override
  public boolean equals(Object rhs) {
    return rhs instanceof TRef && inner.equals(((TRef)rhs).inner);
  }

  @Override
  public String toString() {
    return String.format("&(%s)", inner);
  }

  @Override
  public boolean isOpaque() {
    return true;
  }

  @Override
  public Val defaultValue() {
    throw new PanicException();
  }
}
