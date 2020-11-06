package com.aionescu.tli.ast;

public abstract class Field<F extends Comparable<F>> {
  public static final class FRec extends Field<Ident> {
    private FRec() { }

    @Override
    public boolean equals(Object rhs) {
      return rhs instanceof FRec;
    }
  }
  public static final class FTup extends Field<Integer> {
    private FTup() { }

    @Override
    public boolean equals(Object rhs) {
      return rhs instanceof FTup;
    }
  }

  public static final FRec fRec = new FRec();
  public static final FTup fTup = new FTup();

  private Field() { }
}
