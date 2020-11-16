package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.utils.collections.map.Map;

public final class New implements Stmt {
  private final Ident _ident;
  private final Expr _expr;

  public New(Ident ident, Expr expr) {
    _ident = ident;
    _expr = expr;
  }

  @Override
  public String toString() {
    return String.format("%s = new %s", _ident, _expr);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    throw null;
  }

  @Override
  public ProgState eval(ProgState prog) {
    throw null;
  }
}
