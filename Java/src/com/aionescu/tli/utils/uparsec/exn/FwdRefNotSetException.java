package com.aionescu.tli.utils.uparsec.exn;

public final class FwdRefNotSetException extends UParsecException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "The forward reference was accessed before it was set.";
  }
}
