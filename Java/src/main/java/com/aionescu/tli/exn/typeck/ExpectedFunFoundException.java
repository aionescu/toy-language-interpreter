package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.type.Type;

public final class ExpectedFunFoundException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Type _type;

  public ExpectedFunFoundException(Type type) {
    super();

    _type = type;
  }

  @Override
  public String getMessage() {
    return String.format("Expected function type, but found %s.", _type);
  }
}
