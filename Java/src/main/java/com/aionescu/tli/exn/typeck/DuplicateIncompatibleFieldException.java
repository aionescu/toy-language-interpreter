package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.Ident;

public final class DuplicateIncompatibleFieldException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Ident _ident;

  public DuplicateIncompatibleFieldException(Ident ident) {
    super();

    _ident = ident;
  }

  @Override
  public String getMessage() {
    return String.format("The field %s appears twice in the union, but with different types.", _ident);
  }
}
