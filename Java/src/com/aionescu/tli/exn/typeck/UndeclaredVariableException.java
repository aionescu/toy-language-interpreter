package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.Ident;

public final class UndeclaredVariableException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Ident _ident;

  public UndeclaredVariableException(Ident ident) {
    super();

    _ident = ident;
  }

  @Override
  public String getMessage() {
    return String.format("Variable %s was not declared.", _ident);
  }
}
