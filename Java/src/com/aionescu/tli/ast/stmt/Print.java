package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ThreadState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.utils.data.list.List;
import com.aionescu.tli.utils.data.map.Map;

public final class Print implements Stmt {
  private final Expr _expr;

  public Print(Expr expr) {
    _expr = expr;
  }

  @Override
  public String toString() {
    return String.format("print %s", _expr);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    _expr.typeCheck(sym).mustBeTransparent();
    return sym;
  }

  @Override
  public ThreadState eval(ThreadState prog) {
    return prog.updateGlobal(g -> g.withOut(List.cons(_expr.eval(g.heap, prog.sym), g.out)));
  }
}
