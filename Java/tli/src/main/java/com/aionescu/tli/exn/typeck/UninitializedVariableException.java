package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.Ident;

public final class UninitializedVariableException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Ident _ident;

  public UninitializedVariableException(Ident ident) {
    _ident = ident;
  }

  @Override
  public String getMessage() {
    return String.format("Variable %s was declared, but not initialized.", _ident);
  }
}
