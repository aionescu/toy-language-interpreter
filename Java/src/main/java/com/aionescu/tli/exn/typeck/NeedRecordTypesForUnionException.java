package com.aionescu.tli.exn.typeck;

public final class NeedRecordTypesForUnionException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "Both operands of the \"&\" operator must be of record types.";
  }
}
