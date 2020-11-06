package com.aionescu.tli.ast.type;

import com.aionescu.tli.exn.typeck.TypeMismatchException;

public interface Type {
  boolean isComparable();

  default void expect(Type expected) {
    if (!equals(expected))
      throw new TypeMismatchException(expected, this);
  }
}
