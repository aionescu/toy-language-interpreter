package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.typeck.TypeMismatchException;
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
    return _m.toString();
  }

  @Override
  public int compareTo(Val rhs) {
    if (!(rhs instanceof VRec<?, ?>))
      throw new TypeMismatchException(type(), rhs.type());

    var rec = (VRec<?, ?>)rhs;

    if (!_f.equals(rec._f))
      throw new TypeMismatchException(type(), rhs.type());

    @SuppressWarnings("unchecked")
    var r = Map.compare(_m, (Map<A, Val>)rec._m);

    return r;
  }

  @Override
  public Type type() {
    return new TRec<>(_f, _m.map(Val::type));
  }
}
