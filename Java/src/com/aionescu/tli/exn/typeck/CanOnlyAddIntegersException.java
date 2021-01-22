package com.aionescu.tli.exn.typeck;

public final class CanOnlyAddIntegersException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "Both operands of the addition operation must be integers.";
  }
}
