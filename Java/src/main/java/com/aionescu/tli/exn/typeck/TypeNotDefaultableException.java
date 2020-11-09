package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.type.Type;

public final class TypeNotDefaultableException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Type _type;

  public TypeNotDefaultableException(Type type) {
    super();

    _type = type;
  }

  @Override
  public String getMessage() {
    return String.format("The type %s does not have a default value.", _type);
  }
}
