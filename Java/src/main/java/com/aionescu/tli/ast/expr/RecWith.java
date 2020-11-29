package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VRec;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.ExpectedRecFoundException;
import com.aionescu.tli.utils.collections.map.Map;

public final class RecWith implements Expr {
  private final Expr _lhs;
  private final boolean _isRec;
  private final Map<Field, Expr> _updates;

  public RecWith(Expr lhs, boolean isRec, Map<Field, Expr> updates) {
    _lhs = lhs;
    _isRec = isRec;
    _updates = updates;
  }

  @Override
  public String toString() {
    return String.format("{ %s %s", _lhs, Field.showFields(_updates, _isRec, true, " = "));
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    var t = _lhs.typeCheck(sym);
    var tys = _updates.map(e -> e.typeCheck(sym));

    if (!(t instanceof TRec))
      throw new ExpectedRecFoundException(t);

    var trec = (TRec)t;

    if (_isRec != trec.isRec)
      throw new ExpectedRecFoundException(t);

    tys.toList().iter(p -> Field.checkMember(trec, trec.fields, p.fst, p.snd));
    return t;
  }

  @Override
  public Val eval(Map<Integer, Val> heap, Map<Ident, Val> sym) {
    var vrec = (VRec)_lhs.eval(heap, sym);
    var vals = _updates.map(e -> e.eval(heap, sym));

    return new VRec(_isRec, vals.foldLWithKey((m, k, v) -> m.insert(k, v), vrec.fields));
  }
}
