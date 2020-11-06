package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.*;
import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.kind.ExprKind.R;

public final class BoolLit implements Expr<R> {
  private final boolean _val;

  public BoolLit(boolean val) {
    _val = val;
  }

  @Override
  public String toString() {
    return String.valueOf(_val);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    return TBool.t;
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    return new VBool(_val);
  }
}
