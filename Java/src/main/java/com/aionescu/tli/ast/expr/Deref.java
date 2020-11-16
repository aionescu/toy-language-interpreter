package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.utils.collections.map.Map;

public final class Deref implements Expr {
  private final Expr _ref;

  public Deref(Expr ref) {
    _ref = ref;
  }

  @Override
  public String toString() {
    return String.format("!(%s)", _ref);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    throw null;
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    throw null;
  }
}
