package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.TypeMismatchException;
import com.aionescu.tli.exn.typeck.TypeNotComparableException;
import com.aionescu.tli.exn.typeck.TypeNotDefaultableException;
import com.aionescu.tli.exn.typeck.TypeNotShowableException;

public interface Type {
  boolean isComparable();
  boolean isShowable();
  boolean isDefaultable();

  Val defaultValue();

  default void mustBe(Type expected) {
    if (!equals(expected))
      throw new TypeMismatchException(expected, this);
  }

  default void mustBeComparable() {
    if (!isComparable())
      throw new TypeNotComparableException(this);
  }

  default void mustBeShowable() {
    if (!isShowable())
      throw new TypeNotShowableException(this);
  }

  default void mustBeDefaultable() {
    if (!isDefaultable())
      throw new TypeNotDefaultableException(this);
  }
}
