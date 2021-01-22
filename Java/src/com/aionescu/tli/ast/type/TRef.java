package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.eval.PanicException;

public final class TRef implements Type {
  private final Type _inner;

  public TRef(Type inner) {
    _inner = inner;
  }

  @Override
  public boolean equals(Object rhs) {
    return rhs instanceof TRef && _inner.equals(((TRef)rhs)._inner);
  }

  @Override
  public String toString() {
    return String.format("&(%s)", _inner);
  }

  @Override
  public boolean isOpaque() {
    return true;
  }

  @Override
  public Val defaultValue() {
    throw new PanicException();
  }

  @Override
  public Type unwrapTRef() {
    return _inner;
  }
}
