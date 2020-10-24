package tli.ast.val;

import tli.ast.type.Type;

public final class Bool implements Val {
  public final boolean val;

  public static Bool of(boolean val) {
    return new Bool(val);
  }

  public Bool(boolean val) {
    this.val = val;
  }

  @Override
  public Type type() {
    return Type.BOOL;
  }

  @Override
  public String toString() {
    return val ? "True" : "False";
  }
}
