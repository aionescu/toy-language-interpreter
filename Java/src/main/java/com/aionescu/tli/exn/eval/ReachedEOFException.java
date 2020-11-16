package com.aionescu.tli.exn.eval;

public final class ReachedEOFException extends EvalException {
  private final static long serialVersionUID = 1;

  private final String _file;

  public ReachedEOFException(String file) {
    super();

    _file = file;
  }

  @Override
  public String getMessage() {
    return String.format("There are no more values to read in file %s.", _file);
  }
}
