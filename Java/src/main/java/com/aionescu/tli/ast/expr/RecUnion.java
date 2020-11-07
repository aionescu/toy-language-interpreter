package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.Field.FRec;
import com.aionescu.tli.ast.expr.kind.ExprKind.R;
import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VRec;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.DuplicateIncompatibleFieldException;
import com.aionescu.tli.exn.typeck.NeedRecordTypesForUnionException;
import com.aionescu.tli.utils.collections.map.Map;
import com.aionescu.tli.utils.control.Maybe;

public final class RecUnion implements Expr<R> {
  private final Expr<?> _lhs, _rhs;

  public RecUnion(Expr<?> lhs, Expr<?> rhs) {
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

  private static Type _throwIfDup(Ident i, Maybe<Type> t) {
    return t.match(
      () -> { throw new DuplicateIncompatibleFieldException(i); },
      a -> a);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    // typeCheckExpr sym (RecUnion a b) = do
    //   ta <- typeCheckExpr sym a
    //   tb <- typeCheckExpr sym b
    //   case (ta, tb) of
    //     (TRec FRec as, TRec FRec bs) -> do
    //       (TRec FRec) <$> M.traverseWithKey throwIfDup ((M.unionWith checkDup `on` (Just <$>)) as bs)
    //     _ -> throw $ NeedRecordTypesForUnion

    var ta = _lhs.typeCheck(sym);
    var tb = _rhs.typeCheck(sym);

    try {
      var treca = (TRec<?, ?>)ta;
      var trecb = (TRec<?, ?>)tb;
      var fa = (FRec)treca.f;
      var fb = (FRec)trecb.f;

      fa.equals(fb);

      @SuppressWarnings("unchecked")
      Map<Ident, Maybe<Type>> as = ((Map<Ident, Type>)treca.m).map(Maybe::just);

      @SuppressWarnings("unchecked")
      Map<Ident, Maybe<Type>> bs = ((Map<Ident, Type>)trecb.m).map(Maybe::just);

      return new TRec<>(Field.fRec, as.unionWith(bs, RecUnion::_checkDup).mapWithKey(RecUnion::_throwIfDup));
    } catch (ClassCastException e) {
      throw new NeedRecordTypesForUnionException();
    }
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    var a = _lhs.eval(sym);
    var b = _rhs.eval(sym);

    try {
      var a_ = (VRec<?, ?>)a;
      var b_ = (VRec<?, ?>)b;

      @SuppressWarnings("unchecked")
      Map<Ident, Val> as = (Map<Ident, Val>)a_.m;

      @SuppressWarnings("unchecked")
      Map<Ident, Val> bs = (Map<Ident, Val>)b_.m;

      return new VRec<>(Field.fRec, as.union(bs));
    } catch (ClassCastException e) {
      throw new NeedRecordTypesForUnionException();
    }
  }
}
