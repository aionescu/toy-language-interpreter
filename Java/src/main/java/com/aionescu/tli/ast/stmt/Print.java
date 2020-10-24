package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.VarInfo;
import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.collections.map.Map;

public final class Print implements Stmt {
  private final Expr _expr;

  public static Print of(Expr expr) {
    return new Print(expr);
  }

  public Print(Expr expr) {
    _expr = expr;
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    _expr.typeCheck(sym);
    return sym;
  }

  @Override
  public ProgState eval(ProgState prog) {
    return prog.withOut(List.cons(_expr.eval(prog.sym).toString(), prog.out));
  }

  @Override
  public String toString() {
    return String.format("print %s", _expr);
  }
}
