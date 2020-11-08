package com.aionescu.tli.exn.typeck;

public final class IllFormedASTException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "Ill-formed AST. There may be a bug in the parser.";
  }
}
