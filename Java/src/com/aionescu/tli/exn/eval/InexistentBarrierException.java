package com.aionescu.tli.exn.eval;

public final class InexistentBarrierException extends EvalException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "Attempted to await on a barrier that does not exist.";
  }
}
