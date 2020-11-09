package com.aionescu.tli.exn.typeck;

public final class CanOnlyAppendStringsException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "Both operands of the append operation must be strings.";
  }
}
