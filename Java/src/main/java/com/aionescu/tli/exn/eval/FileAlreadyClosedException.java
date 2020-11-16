package com.aionescu.tli.exn.eval;

public final class FileAlreadyClosedException extends EvalException {
  private final static long serialVersionUID = 1;

  private final String _file;

  public FileAlreadyClosedException(String file) {
    super();

    _file = file;
  }

  @Override
  public String getMessage() {
    return String.format("The file %s has been closed.", _file);
  }
}
