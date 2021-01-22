package com.aionescu.tli.exn.eval;

import com.aionescu.tli.ast.val.VStr;

public final class FileNotOpenedException extends EvalException {
  private final static long serialVersionUID = 1;

  private final String _file;

  public FileNotOpenedException(String file) {
    super();

    _file = file;
  }

  @Override
  public String getMessage() {
    return String.format("The file %s has not been opened.", VStr.escapeString(_file));
  }
}
