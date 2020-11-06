package com.aionescu.tli.ast.expr.kind;

public abstract class ExprKind {
  public static final class L extends ExprKind {
    private L() { }

    @Override
    public boolean equals(Object rhs) {
      return rhs instanceof L;
    }
  }

  public static final class R extends ExprKind {
    private R() { }

    @Override
    public boolean equals(Object rhs) {
      return rhs instanceof R;
    }
  }

  public static final L l = new L();
  public static final R r = new R();

  private ExprKind() { }
}
