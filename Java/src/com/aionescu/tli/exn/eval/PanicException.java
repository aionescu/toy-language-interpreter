package com.aionescu.tli.exn.eval;

public final class PanicException extends EvalException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "Panic! The 'impossible' happened. Did you run the typechecker?";
  }
}
