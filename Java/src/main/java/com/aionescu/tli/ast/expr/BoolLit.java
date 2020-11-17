package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.*;
import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;

public final class BoolLit implements Expr {
  private final boolean _val;

  public BoolLit(boolean val) {
    _val = val;
  }

  @Override
  public String toString() {
    return _val ? "True" : "False";
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    return TBool.t;
  }

  @Override
  public Val eval(Map<Integer, Val> heap, Map<Ident, Val> sym) {
    return new VBool(_val);
  }
}
