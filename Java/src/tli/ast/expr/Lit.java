package tli.ast.expr;

import tli.ast.type.Type;
import tli.ast.val.*;
import tli.ast.varstate.VarState;
import utils.collections.map.Map;

import tli.ast.Ident;

public final class Lit implements Expr {
  private final Val _val;

  public static Lit of(Val val) {
    return new Lit(val);
  }

  public Lit(Val val) {
    _val = val;
  }

  @Override
  public Type typeCheck(Map<Ident, Type> sym) {
    return _val.type();
  }

  @Override
  public Val eval(Map<Ident, VarState> sym) {
    return _val;
  }

  @Override
  public String toString() {
    return _val.toString();
  }
}
