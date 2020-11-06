package com.aionescu.tli.ast.type;

public final class TFun implements Type {
  private final Type _in, _out;

  public TFun(Type in, Type out) {
    _in = in;
    _out = out;
  }

  @Override
  public boolean equals(Object rhs) {
    if (!(rhs instanceof TFun))
      return false;

    var fun = (TFun)rhs;
    return _in.equals(fun._in) && _out.equals(fun._out);
  }

  @Override
  public String toString() {
    return String.format("(%s -> %s)", _in, _out);
  }

  @Override
  public boolean isComparable() {
    return false;
  }
}
