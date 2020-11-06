package com.aionescu.tli.exn.eval;

public final class InvalidComparisonException extends EvalException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "Invalid comparison. Did you run the typechecker?";
  }
}
