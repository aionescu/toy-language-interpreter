package tli.ast.expr;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.type.Type;
import tli.ast.type.VarInfo;
import tli.ast.val.Val;
import tli.ast.type.VarState;
import tli.exn.typeck.UndeclaredVariableException;
import tli.exn.typeck.UninitializedVariableException;

public final class Var implements Expr {
  private final Ident _ident;

  public static Var of(Ident ident) {
    return new Var(ident);
  }

  public Var(Ident ident) {
    _ident = ident;
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    var info = sym.lookup(_ident).orElseGet(() -> {
      throw new UndeclaredVariableException(_ident);
    });

    if (info.state == VarState.UNINIT)
      throw new UninitializedVariableException(_ident);

    return info.type;
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    return sym.lookup(_ident).get();
  }

  @Override
  public String toString() {
    return _ident.toString();
  }
}
