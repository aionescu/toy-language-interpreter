package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.type.Type;

public abstract class Val implements Comparable<Val> {
  @Override
  public final boolean equals(Object rhs) {
    return getClass().equals(rhs.getClass()) && compareTo((Val)rhs) == 0;
  }

  public abstract Type type();
}
