package com.aionescu.tli.ast.val;

import java.util.function.UnaryOperator;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.prog.GCStats;
import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.eval.PanicException;
import com.aionescu.tli.utils.collections.map.Map;
import com.aionescu.tli.utils.collections.set.Set;

public final class VRec extends Val {
  public final boolean isRec;
  public final Map<Field, Val> fields;

  public VRec(boolean isRec, Map<Field, Val> fields) {
    this.isRec = isRec;
    this.fields = fields;
  }

  @Override
  public String toString() {
    return Field.showFields(fields, isRec, false, " = ");
  }

  @Override
  public int compareTo(Val rhs) {
    if (!(rhs instanceof VRec))
      throw new PanicException();

    var rec = (VRec)rhs;

    if (isRec != rec.isRec)
      throw new PanicException();

    return Map.compare(fields, rec.fields);
  }

  @Override
  public Type type() {
    return new TRec(isRec, fields.map(Val::type));
  }

  @Override
  public Set<Integer> getInnerAddrs() {
    return GCStats.getInnerAddrsScope(fields);
  }

  @Override
  public Val mapInnerAddrs(UnaryOperator<Integer> f) {
    return new VRec(isRec, GCStats.mapInnerAddrsMap(f, fields));
  }
}
