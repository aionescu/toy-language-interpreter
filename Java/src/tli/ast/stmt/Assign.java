package tli.ast.stmt;

import tli.ast.Ident;
import tli.ast.expr.Expr;
import tli.ast.prog.ProgState;
import tli.ast.type.VarInfo;
import tli.ast.type.VarState;
import tli.exn.typeck.UndeclaredVariableException;
import utils.collections.map.Map;

public final class Assign implements Stmt {
  private final Ident _ident;
  private final Expr _expr;

  public static Assign of(Ident ident, Expr expr) {
    return new Assign(ident, expr);
  }

  public Assign(Ident ident, Expr expr) {
    _ident = ident;
    _expr = expr;
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    var info = sym.lookup(_ident).orElseGet(() -> {
      throw new UndeclaredVariableException(_ident);
    });

    _expr.typeCheck(sym).expect(info.type);
    return sym.insert(_ident, VarInfo.of(info.type, VarState.INIT));
  }

  @Override
  public ProgState eval(ProgState prog) {
    return prog.withSym(prog.sym.insert(_ident, _expr.eval(prog.sym)));
  }

  @Override
  public String toString() {
    return String.format("%s <- %s", _ident, _expr);
  }
}
