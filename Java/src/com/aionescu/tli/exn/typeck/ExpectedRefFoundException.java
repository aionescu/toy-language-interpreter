package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.type.Type;

public final class ExpectedRefFoundException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Type _type;

  public ExpectedRefFoundException(Type type) {
    super();

    _type = type;
  }

  @Override
  public String getMessage() {
    return String.format("Expected reference type, but found type %s.", _type);
  }
}
