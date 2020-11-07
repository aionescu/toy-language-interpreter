package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.Field.FRec;
import com.aionescu.tli.ast.Field.FTup;
import com.aionescu.tli.ast.expr.kind.ExprKind.R;
import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VRec;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.ExpectedRecFoundException;
import com.aionescu.tli.utils.collections.map.Map;

public final class RecWith<F extends Field<A>, A extends Comparable<A>> implements Expr<R> {
  private final Expr<?> _lhs;
  private final F _f;
  private final Map<A, Expr<R>> _m;

  public RecWith(Expr<?> lhs, F f, Map<A, Expr<R>> m) {
    _lhs = lhs;
    _f = f;
    _m = m;
  }

  @Override
  public String toString() {
    return String.format("{ %s %s", _lhs, _f.showFields(_m, true, " <- "));
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    var t = _lhs.typeCheck(sym);
    var tys = _m.map(e -> e.typeCheck(sym));

    if (!(t instanceof TRec<?, ?>))
      throw new ExpectedRecFoundException(t);

    var trec = (TRec<?, ?>)t;

    if (!_f.equals(trec.f))
      throw new ExpectedRecFoundException(t);

    if (_f instanceof FRec) {
      var f = (FRec)_f;

      @SuppressWarnings("unchecked")
      var tys_ = (Map<Ident, Type>)tys;

      @SuppressWarnings("unchecked")
      var trec_ = (TRec<FRec, Ident>)trec;

      tys_.toList().iter(p -> f.checkMember(trec_, trec_.m, p.fst, p.snd));
    } else if (_f instanceof FTup) {
      var f = (FTup)_f;

      @SuppressWarnings("unchecked")
      var tys_ = (Map<Integer, Type>)tys;

      @SuppressWarnings("unchecked")
      var trec_ = (TRec<FTup, Integer>)trec;

      tys_.toList().iter(p -> f.checkMember(trec_, trec_.m, p.fst, p.snd));
    } else
      throw new IllegalStateException("Is there a 3rd record kind?");

    return t;
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    var v = _lhs.eval(sym);
    var vals = _m.map(e -> e.eval(sym));

    if (!(v instanceof VRec<?, ?>))
      throw new IllegalStateException("Expected record in RecWith. Did you run the typechecker?");

    var vrec = (VRec<?, ?>)v;

    if (!_f.equals(vrec.f))
      throw new IllegalStateException("Incompatible record kinds in RecWith. Did you run the typechecker?");

    if (_f instanceof FRec) {
      var f = (FRec)_f;

      @SuppressWarnings("unchecked")
      var vals_ = (Map<Ident, Val>)vals;

      @SuppressWarnings("unchecked")
      var vrec_ = (VRec<FRec, Ident>)vrec;

      return new VRec<>(f, vals_.foldlWithKey((m, k, v_) -> m.insert(k, v_), vrec_.m));
    } else if (_f instanceof FTup) {
      var f = (FTup)_f;

      @SuppressWarnings("unchecked")
      var vals_ = (Map<Integer, Val>)vals;

      @SuppressWarnings("unchecked")
      var vrec_ = (VRec<FTup, Integer>)vrec;

      return new VRec<>(f, vals_.foldlWithKey((m, k, v_) -> m.insert(k, v_), vrec_.m));
    } else
      throw new IllegalStateException("Is there a 3rd record kind?");
  }
}
