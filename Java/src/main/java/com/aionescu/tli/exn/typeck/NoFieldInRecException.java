package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.Field.RecField;
import com.aionescu.tli.ast.type.Type;

public final class NoFieldInRecException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Type _type;
  private final Field _idx;

  public NoFieldInRecException(Type type, Field idx) {
    super();

    _type = type;
    _idx = idx;
  }

  @Override
  public String getMessage() {
    return
      _idx instanceof RecField
      ? String.format("The record type %s has no field named %s.", _type, _idx)
      : String.format("The tuple type %s does not have enough elements to be indexed by the index %s", _type, _idx);
  }
}
