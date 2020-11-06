package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.type.varinfo.VarState;
import com.aionescu.tli.exn.typeck.UndeclaredVariableException;
import com.aionescu.tli.utils.collections.map.Map;

public final class Assign implements Stmt {
  private final Ident _ident;
  private final Expr _expr;

  public Assign(Ident ident, Expr expr) {
    _ident = ident;
    _expr = expr;
  }

  @Override
  public String toString() {
    return String.format("%s <- %s", _ident, _expr);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    var info = sym.lookup(_ident).match(
      () -> { throw new UndeclaredVariableException(_ident); },
      a -> a);

    _expr.typeCheck(sym).expect(info.type);
    return sym.insert(_ident, new VarInfo(info.type, VarState.INIT));
  }

  @Override
  public ProgState eval(ProgState prog) {
    return prog.withSym(prog.sym.insert(_ident, _expr.eval(prog.sym)));
  }
}
