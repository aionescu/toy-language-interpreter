package com.aionescu.tli.ast.expr;

import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.VarInfo;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.ast.type.VarState;
import com.aionescu.tli.exn.typeck.UndeclaredVariableException;
import com.aionescu.tli.exn.typeck.UninitializedVariableException;

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
