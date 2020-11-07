package com.aionescu.tli.ast;

import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.typeck.NoFieldInRecException;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.map.Map;

public abstract class Field<F extends Comparable<F>> {
  public static final class FRec extends Field<Ident> {
    private FRec() { }

    @Override
    public boolean equals(Object rhs) {
      return rhs instanceof FRec;
    }

    @Override
    public <A> String showFields(Map<Ident, A> m, boolean isWithExpr, String sep) {
      return m.toString(isWithExpr ? "| " : "{ ", " }", sep);
    }

    @Override
    public void checkMember(Type trec, Map<Ident, Type> m, Ident idx, Type t) {
      m.lookup(idx).matchDo(
        () -> { throw new NoFieldInRecException(trec, this, idx); },
        t_ -> t.expect(t_));
    }
  }

  public static final class FTup extends Field<Integer> {
    private FTup() { }

    @Override
    public boolean equals(Object rhs) {
      return rhs instanceof FTup;
    }

    @Override
    public <A> String showFields(Map<Integer, A> m, boolean isWithExpr, String sep) {
      if (isWithExpr)
        return m.toString("| ", " }", sep);

      var l = m.toList().<A>map(Pair::snd_);
      return l.toString("(", l.length() == 1 ? ",)" : ")");
    }

    @Override
    public void checkMember(Type trec, Map<Integer, Type> m, Integer idx, Type t) {
      m.lookup(idx).matchDo(
        () -> { throw new NoFieldInRecException(trec, this, idx); },
        t_ -> t.expect(t_));
    }
  }

  public static final FRec fRec = new FRec();
  public static final FTup fTup = new FTup();

  private Field() { }

  public abstract <A> String showFields(Map<F, A> m, boolean isWithExpr, String sep);

  public abstract void checkMember(Type trec, Map<F, Type> m, F idx, Type t);
}
