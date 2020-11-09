package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.exn.eval.InvalidComparisonException;
import com.aionescu.tli.utils.collections.map.Map;

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
      throw new InvalidComparisonException();

    var rec = (VRec)rhs;

    if (isRec != rec.isRec)
      throw new InvalidComparisonException();

    return Map.compare(fields, rec.fields);
  }
}
