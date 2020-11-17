package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.type.TStr;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.*;
import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;

public final class StrLit implements Expr {
  private final String _val;

  public StrLit(String val) {
    _val = val;
  }

  @Override
  public String toString() {
    return VStr.escapeString(_val);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    return TStr.t;
  }

  @Override
  public Val eval(Map<Integer, Val> heap, Map<Ident, Val> sym) {
    return new VStr(_val);
  }
}
