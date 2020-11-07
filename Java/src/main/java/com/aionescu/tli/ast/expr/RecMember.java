package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.Field.FRec;
import com.aionescu.tli.ast.Field.FTup;
import com.aionescu.tli.ast.expr.kind.ExprKind;
import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VRec;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.ExpectedRecFoundException;
import com.aionescu.tli.exn.typeck.NoFieldInRecException;
import com.aionescu.tli.utils.collections.map.Map;

public final class RecMember<F extends Field<A>, A extends Comparable<A>, K extends ExprKind> implements Expr<K> {
  private final Expr<K> _lhs;
  private final F _f;
  private final A _idx;

  public RecMember(Expr<K> lhs, F f, A idx) {
    _lhs = lhs;
    _f = f;
    _idx = idx;
  }

  @Override
  public String toString() {
    return String.format("%s.%s", _lhs, _idx);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    var t = _lhs.typeCheck(sym);

    if (!(t instanceof TRec<?, ?>))
      throw new ExpectedRecFoundException(t);

    var trec = (TRec<?, ?>)t;

    if (!_f.equals(trec.f))
      throw new ExpectedRecFoundException(t);

    @SuppressWarnings("unchecked")
    var result = ((Map<A, Type>)trec.m).lookup(_idx);

    return result.match(() -> { throw new NoFieldInRecException(t, _f, _idx); }, a -> a);
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    var v = _lhs.eval(sym);

    if (!(v instanceof VRec<?, ?>))
      throw new IllegalStateException("Expected record in RecMember. Did you run the typechecker?");

    var vrec = (VRec<?, ?>)v;

    if (!_f.equals(vrec.f))
      throw new IllegalStateException("Incompatible record kinds in RecWith. Did you run the typechecker?");

    if (_f instanceof FRec) {
      @SuppressWarnings("unchecked")
      var vrec_ = (VRec<FRec, Ident>)vrec;

      return vrec_.m.lookup((Ident)_idx).unwrap();
    } else if (_f instanceof FTup) {
      @SuppressWarnings("unchecked")
      var vrec_ = (VRec<FTup, Integer>)vrec;

      return vrec_.m.lookup((Integer)_idx).unwrap();
    } else
      throw new IllegalStateException("Is there a 3rd record kind?");
  }
}
