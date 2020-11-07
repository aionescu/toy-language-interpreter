package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.map.Map;

public final class TRec<F extends Field<A>, A extends Comparable<A>> implements Type {
  public final F f;
  public final Map<A, Type> m;

  public TRec(F f, Map<A, Type> m) {
    this.f = f;
    this.m = m;
  }

  @Override
  public boolean equals(Object rhs) {
    if (!(rhs instanceof TRec<?, ?>))
      return false;

    var rec = (TRec<?, ?>)rhs;
    return f.equals(rec.f) && m.equals(rec.m);
  }

  @Override
  public String toString() {
    return f.showFields(m, false, " : ");
  }

  @Override
  public boolean isComparable() {
    return m.toList().map(Pair::snd_).all(Type::isComparable);
  }
}
