package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VRec;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.utils.collections.map.Map;

public final class RecLit implements Expr {
  private final boolean _isRec;
  private final Map<Field, Expr> _fields;

  public RecLit(boolean isRec, Map<Field, Expr> fields) {
    _isRec = isRec;
    _fields = fields;
  }

  @Override
  public String toString() {
    return Field.showFields(_fields, _isRec, false, " <- ");
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    return new TRec(_isRec, _fields.map(e -> e.typeCheck(sym)));
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    return new VRec(_isRec, _fields.map(e -> e.eval(sym)));
  }
}
