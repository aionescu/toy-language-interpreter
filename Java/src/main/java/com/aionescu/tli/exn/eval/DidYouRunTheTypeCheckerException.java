package com.aionescu.tli.exn.eval;

public final class DidYouRunTheTypeCheckerException extends EvalException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "The impossible happened. Did you run the typechecker?";
  }
}
