package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.utils.data.map.Map;

public final class Default implements Expr {
  private final Type _type;

  public Default(Type type) {
    _type = type;
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    _type.mustBeTransparent();
    return _type;
  }

  @Override
  public Val eval(Map<Integer, Val> heap, Map<Ident, Val> sym) {
    return _type.defaultValue();
  }
}
