package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.utils.collections.map.Map;

public final class WriteAt implements Stmt {
  private final Expr _lhs, _rhs;

  public WriteAt(Expr lhs, Expr rhs) {
    _lhs = lhs;
    _rhs = rhs;
  }

  @Override
  public String toString() {
    return String.format("%s := %s", _lhs, _rhs);
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
