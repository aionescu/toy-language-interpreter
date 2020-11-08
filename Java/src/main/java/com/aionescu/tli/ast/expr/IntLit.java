package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.type.TInt;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.*;
import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;

public final class IntLit implements Expr {
  private final int _val;

  public IntLit(int val) {
    _val = val;
  }

  @Override
  public String toString() {
    return String.valueOf(_val);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    return TInt.t;
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    return new VInt(_val);
  }
}
