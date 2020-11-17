package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VRec;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.DuplicateIncompatibleFieldException;
import com.aionescu.tli.exn.typeck.NeedRecordTypesForUnionException;
import com.aionescu.tli.utils.collections.map.Map;
import com.aionescu.tli.utils.control.Maybe;

public final class RecUnion implements Expr {
  private final Expr _lhs, _rhs;

  public RecUnion(Expr lhs, Expr rhs) {
    _lhs = lhs;
    _rhs = rhs;
  }

  @Override
  public String toString() {
    return String.format("(%s & %s)", _lhs, _rhs);
  }

  private static Maybe<Type> _checkDup(Maybe<Type> a, Maybe<Type> b) {
    return a.equals(b) ? a : Maybe.nothing();
  }

  private static Type _throwIfDup(Field f, Maybe<Type> t) {
    return t.match(
      () -> { throw new DuplicateIncompatibleFieldException(f); },
      a -> a);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    var ta = _lhs.typeCheck(sym);
    var tb = _rhs.typeCheck(sym);

    if (!(ta instanceof TRec && tb instanceof TRec))
      throw new NeedRecordTypesForUnionException();

    var treca = (TRec)ta;
    var trecb = (TRec)tb;

    if (!treca.isRec || !trecb.isRec)
      throw new NeedRecordTypesForUnionException();

    var as = treca.fields.map(Maybe::just);
    var bs = trecb.fields.map(Maybe::just);

    return new TRec(true, as.unionWith(bs, RecUnion::_checkDup).mapWithKey(RecUnion::_throwIfDup));
  }

  @Override
  public Val eval(Map<Integer, Val> heap, Map<Ident, Val> sym) {
    var as = ((VRec)_lhs.eval(heap, sym)).fields;
    var bs = ((VRec)_rhs.eval(heap, sym)).fields;

    return new VRec(true, as.union(bs));
  }
}
