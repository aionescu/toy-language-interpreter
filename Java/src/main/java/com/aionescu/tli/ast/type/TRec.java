package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.val.VRec;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.data.map.Map;

public final class TRec implements Type {
  public boolean isRec;
  public final Map<Field, Type> fields;

  public TRec(boolean isRec, Map<Field, Type> fields) {
    this.isRec = isRec;
    this.fields = fields;
  }

  @Override
  public boolean equals(Object rhs) {
    return rhs instanceof TRec && fields.equals(((TRec)rhs).fields);
  }

  @Override
  public String toString() {
    return Field.showFields(fields, isRec, false, ": ");
  }

  @Override
  public boolean isOpaque() {
    return fields.toList().map(Pair::snd_).any(Type::isOpaque);
  }

  @Override
  public Val defaultValue() {
    return new VRec(isRec, fields.map(Type::defaultValue));
  }
}
