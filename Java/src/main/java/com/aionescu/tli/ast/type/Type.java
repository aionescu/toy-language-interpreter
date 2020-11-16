package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.TypeMismatchException;
import com.aionescu.tli.exn.typeck.TypeIsOpaqueException;

public interface Type {
  boolean isOpaque();

  Val defaultValue();

  default void mustBe(Type expected) {
    if (!equals(expected))
      throw new TypeMismatchException(expected, this);
  }

  default void mustBeTransparent() {
    if (isOpaque())
      throw new TypeIsOpaqueException(this);
  }
}
