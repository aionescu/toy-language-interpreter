package com.aionescu.tli.ast.expr;

import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.ast.type.varinfo.VarState;
import com.aionescu.tli.exn.typeck.UndeclaredVariableException;
import com.aionescu.tli.exn.typeck.UninitializedVariableException;

public final class Var implements Expr {
  public final Ident ident;

  public Var(Ident ident) {
    this.ident = ident;
  }

  @Override
  public String toString() {
    return ident.toString();
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    var info = sym.lookup(ident).match(
      () -> { throw new UndeclaredVariableException(ident); },
      a -> a);

    if (info.state == VarState.UNINIT)
      throw new UninitializedVariableException(ident);

    return info.type;
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    return sym.lookup(ident).unwrap();
  }
}
