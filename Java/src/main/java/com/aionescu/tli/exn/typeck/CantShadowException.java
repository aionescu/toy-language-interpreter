package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.Ident;

public final class CantShadowException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Ident _ident;

  public CantShadowException(Ident ident) {
    super();

    _ident = ident;
  }

  @Override
  public String getMessage() {
    return String.format("Lambda argument cannot shadow existing variable %s.", _ident);
  }
}
