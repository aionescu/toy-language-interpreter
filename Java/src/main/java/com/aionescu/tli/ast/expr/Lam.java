package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.TFun;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.type.varinfo.VarState;
import com.aionescu.tli.ast.val.VFun;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.CantShadowException;
import com.aionescu.tli.utils.data.map.Map;

public final class Lam implements Expr {
  private final Ident _argName;
  private final Type _argType;
  private final Expr _body;

  public Lam(Ident argName, Type argType, Expr body) {
    _argName = argName;
    _argType = argType;
    _body = body;
  }

  @Override
  public String toString() {
    return String.format("((%s: %s) -> %s)", _argName, _argType, _body);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    return sym.lookup(_argName).match(
      () -> new TFun(_argType, _body.typeCheck(sym.insert(_argName, new VarInfo(_argType, VarState.INIT)))),
      t -> { throw new CantShadowException(_argName); });
  }

  @Override
  public Val eval(Map<Integer, Val> heap, Map<Ident, Val> sym) {
    return new VFun(sym, (heap_, sym_, a) -> _body.eval(heap_, sym_.insert(_argName, a)));
  }
}
