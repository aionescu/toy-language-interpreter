package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.map.Map;

public final class TRec<F extends Field<A>, A extends Comparable<A>> implements Type {
  private final F _f;
  private final Map<A, Type> _m;

  public TRec(F f, Map<A, Type> m) {
    _f = f;
    _m = m;
  }

  @Override
  public boolean equals(Object rhs) {
    if (!(rhs instanceof TRec<?, ?>))
      return false;

    var rec = (TRec<?, ?>)rhs;
    return _f.equals(rec._f) && _m.equals(rec._m);
  }

  @Override
  public String toString() {
    if (_f.equals(Field.fRec))
      return _m.toString("{ ", " }", " : ");

    var ts = _m.toList().map(Pair::snd_);
    return ts.toString("(", ts.length() == 1 ? ",)" : ")");
  }

  @Override
  public boolean isComparable() {
    return _m.toList().map(Pair::snd_).all(Type::isComparable);
  }
}
