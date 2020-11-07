package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.kind.ExprKind.R;
import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VRec;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.utils.collections.map.Map;

public final class RecLit<F extends Field<A>, A extends Comparable<A>> implements Expr<R> {
  private final F _f;
  private final Map<A, Expr<?>> _m;

  public RecLit(F f, Map<A, Expr<?>> m) {
    _f = f;
    _m = m;
  }

  @Override
  public String toString() {
    return _f.showFields(_m, false, " <- ");
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    return new TRec<>(_f, _m.map(e -> e.typeCheck(sym)));
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    return new VRec<>(_f, _m.map(e -> e.eval(sym)));
  }
}
