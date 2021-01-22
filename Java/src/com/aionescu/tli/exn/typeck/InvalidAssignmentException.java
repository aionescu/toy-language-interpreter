package com.aionescu.tli.exn.typeck;

public final class InvalidAssignmentException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "The left-hand side of an assignment must be an lvalue-expression.";
  }
}
