package com.aionescu.tli.exn.typeck;

import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;

public final class ExpectedRecFoundException extends TypeCheckerException {
  private final static long serialVersionUID = 1;

  private final Type _type;

  public ExpectedRecFoundException(Type type) {
    super();

    _type = type;
  }

  @Override
  public String getMessage() {
    if (!(_type instanceof TRec))
      return String.format("Expected tuple or record type, but found %s.", _type);

    var trec = (TRec)_type;

    return
      trec.isRec
      ? String.format("Expected tuple type, but found record type %s.", _type)
      : String.format("Expected record type, but found tuple type %s.", _type);
  }
}
