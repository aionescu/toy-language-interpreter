package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.VarInfo;
import com.aionescu.tli.ast.val.*;
import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;

public final class Lit implements Expr {
  private final Val _val;

  public Lit(Val val) {
    _val = val;
  }

  @Override
  public String toString() {
    return _val.toString();
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    return _val.type();
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    return _val;
  }
}
