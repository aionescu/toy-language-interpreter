package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.type.Type;

public final class TypeNotComparableException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Type _type;

  public TypeNotComparableException(Type type) {
    super();

    _type = type;
  }

  @Override
  public String getMessage() {
    return String.format("Values of type %s cannot be compared.", _type);
  }
}
