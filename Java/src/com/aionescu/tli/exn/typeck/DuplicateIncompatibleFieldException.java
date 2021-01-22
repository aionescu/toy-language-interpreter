package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.Field;

public final class DuplicateIncompatibleFieldException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Field _field;

  public DuplicateIncompatibleFieldException(Field field) {
    super();

    _field = field;
  }

  @Override
  public String getMessage() {
    return String.format("The field %s appears twice in the union, but with different types.", _field);
  }
}
