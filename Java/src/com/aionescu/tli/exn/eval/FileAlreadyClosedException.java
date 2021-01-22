package com.aionescu.tli.exn.eval;

import com.aionescu.tli.ast.val.VStr;

public final class FileAlreadyClosedException extends EvalException {
  private final static long serialVersionUID = 1;

  private final String _file;

  public FileAlreadyClosedException(String file) {
    super();

    _file = file;
  }

  @Override
  public String getMessage() {
    return String.format("The file %s has been closed.", VStr.escapeString(_file));
  }
}
