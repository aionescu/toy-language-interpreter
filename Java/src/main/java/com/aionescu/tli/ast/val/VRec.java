package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.exn.eval.InvalidComparisonException;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.map.Map;

public final class VRec<F extends Field<A>, A extends Comparable<A>> implements Val {
  private final F _f;
  private final Map<A, Val> _m;

  public VRec(F f, Map<A, Val> m) {
    _f = f;
    _m = m;
  }

  @Override
  public boolean equals(Object rhs) {
    if (!(rhs instanceof VRec<?, ?>))
      return false;

    var rec = (VRec<?, ?>)rhs;
    return _f.equals(rec._f) && _m.equals(rec._m);
  }

  @Override
  public String toString() {
    if (_f.equals(Field.fRec))
      return _m.toString();

    var ts = _m.toList().map(Pair::snd_);
    return ts.toString("(", ts.length() == 1 ? ",)" : ")");
  }

  @Override
  public int compareTo(Val rhs) {
    if (!(rhs instanceof VRec<?, ?>))
      throw new InvalidComparisonException();

    var rec = (VRec<?, ?>)rhs;

    if (!_f.equals(rec._f))
      throw new InvalidComparisonException();

    @SuppressWarnings("unchecked")
    var r = Map.compare(_m, (Map<A, Val>)rec._m);

    return r;
  }
}
