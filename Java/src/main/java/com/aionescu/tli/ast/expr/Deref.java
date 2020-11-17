package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VRef;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.utils.collections.map.Map;

public final class Deref implements Expr {
  private final Expr _ref;

  public Deref(Expr ref) {
    _ref = ref;
  }

  @Override
  public String toString() {
    return String.format("!(%s)", _ref);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    return _ref.typeCheck(sym).unwrapTRef();
  }

  @Override
  public Val eval(Map<Integer, Val> heap, Map<Ident, Val> sym) {
    var v = ((VRef)_ref.eval(heap, sym)).addr;
    return heap.lookup(v).unwrap();
  }
}
