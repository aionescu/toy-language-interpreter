package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VBool;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.utils.data.map.Map;

public final class Not implements Expr {
  private final Expr _expr;

  public Not(Expr expr) {
    _expr = expr;
  }

  @Override
  public String toString() {
    return String.format("(not %s)", _expr);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    _expr.typeCheck(sym).mustBe(TBool.t);
    return TBool.t;
  }

  @Override
  public Val eval(Map<Integer, Val> heap, Map<Ident, Val> sym) {
    return new VBool(!((VBool)_expr.eval(heap, sym)).val);
  }
}
