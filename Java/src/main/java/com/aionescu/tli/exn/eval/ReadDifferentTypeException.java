package com.aionescu.tli.exn.eval;

import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.val.VStr;

public final class ReadDifferentTypeException extends EvalException {
  private final static long serialVersionUID = 1;

  private final String _file;
  private final Type _found;
  private final Type _expected;

  public ReadDifferentTypeException(String file, Type found, Type expected) {
    super();

    _file = file;
    _found = found;
    _expected = expected;
  }

  @Override
  public String getMessage() {
    return String.format("In file %s, found a value of type %s, but expected a value of type %s.",
      VStr.escapeString(_file), _found, _expected);
  }
}
