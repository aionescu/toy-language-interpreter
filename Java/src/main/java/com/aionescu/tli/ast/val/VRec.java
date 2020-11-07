package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.exn.eval.InvalidComparisonException;
import com.aionescu.tli.utils.collections.map.Map;

public final class VRec<F extends Field<A>, A extends Comparable<A>> implements Val {
  public final F f;
  public final Map<A, Val> m;

  public VRec(F f, Map<A, Val> m) {
    this.f = f;
    this.m = m;
  }

  @Override
  public boolean equals(Object rhs) {
    if (!(rhs instanceof VRec<?, ?>))
      return false;

    var rec = (VRec<?, ?>)rhs;
    return f.equals(rec.f) && m.equals(rec.m);
  }

  @Override
  public String toString() {
    return f.showFields(m, false, " <- ");
  }

  @Override
  public int compareTo(Val rhs) {
    if (!(rhs instanceof VRec<?, ?>))
      throw new InvalidComparisonException();

    var rec = (VRec<?, ?>)rhs;

    if (!f.equals(rec.f))
      throw new InvalidComparisonException();

    @SuppressWarnings("unchecked")
    var r = Map.compare(m, (Map<A, Val>)rec.m);

    return r;
  }
}
