package com.aionescu.tli.ast;

public final class Ident implements Comparable<Ident> {
  public final String name;

  public Ident(String name) {
    this.name = name;
  }

  @Override
  public int hashCode() {
    return name.hashCode();
  }

  @Override
  public boolean equals(Object rhs) {
    return rhs instanceof Ident && name.equals(((Ident)rhs).name);
  }

  @Override
  public String toString() {
    return name;
  }

  @Override
  public int compareTo(Ident rhs) {
    return name.compareTo(rhs.name);
  }
}
