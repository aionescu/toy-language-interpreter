package com.aionescu.tli.exn.eval;

import com.aionescu.tli.ast.val.VStr;

public final class ReachedEOFException extends EvalException {
  private final static long serialVersionUID = 1;

  private final String _file;

  public ReachedEOFException(String file) {
    super();

    _file = file;
  }

  @Override
  public String getMessage() {
    return String.format("There are no more values to read in file %s.", VStr.escapeString(_file));
  }
}
