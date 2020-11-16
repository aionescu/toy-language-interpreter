package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.type.Type;

public final class TypeIsOpaqueException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Type _type;

  public TypeIsOpaqueException(Type type) {
    super();

    _type = type;
  }

  @Override
  public String getMessage() {
    return String.format("The type %s is opaque.", _type);
  }
}
