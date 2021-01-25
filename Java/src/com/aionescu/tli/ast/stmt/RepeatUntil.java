package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.expr.Not;
import com.aionescu.tli.ast.prog.ThreadState;
import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.utils.data.map.Map;

public final class RepeatUntil implements Stmt {
  private final Stmt _stmt;
  private final Expr _expr;

  public RepeatUntil(Stmt stmt, Expr expr) {
    _stmt = stmt;
    _expr = expr;
  }

  @Override
  public String toString() {
    return String.format("repeat { %s } until %s", _stmt, _expr);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    _stmt.typeCheck(sym);
    _expr.typeCheck(sym).mustBe(TBool.t);
    return sym;
  }

  @Override
  public ThreadState eval(ThreadState prog) {
    return prog.withToDo(prog.toDo.push(new Seq(_stmt, new While(new Not(_expr), _stmt))));
  }
}
