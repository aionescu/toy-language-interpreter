package com.aionescu.tli.ast.type;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.ExpectedRefFoundException;
import com.aionescu.tli.exn.typeck.TypeIsOpaqueException;
import com.aionescu.tli.exn.typeck.TypeMismatchException;
import com.aionescu.tli.exn.typeck.UndeclaredVariableException;
import com.aionescu.tli.utils.data.map.Map;

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

  default Type unwrapTRef() {
    throw new ExpectedRefFoundException(this);
  }

  static Type lookupVariable(Map<Ident, VarInfo> sym, Ident ident) {
    return sym.lookup(ident).unwrap(() -> new UndeclaredVariableException(ident)).type;
  }
}
