package com.aionescu.tli.ast;

import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.typeck.IllFormedASTException;
import com.aionescu.tli.exn.typeck.NoFieldInRecException;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.map.Map;

public abstract class Field implements Comparable<Field> {
  public static final class RecField extends Field {
    public final Ident f;

    public RecField(Ident f) {
      this.f = f;
    }

    @Override
    public boolean equals(Object rhs) {
      return rhs instanceof RecField && f.equals(((RecField)rhs).f);
    }

    @Override
    public String toString() {
      return f.toString();
    }

    @Override
    public int compareTo(Field rhs) {
      if (!(rhs instanceof RecField))
        throw new IllFormedASTException();

      return f.compareTo(((RecField)rhs).f);
    }

    @Override
    public boolean isRecField() {
      return true;
    }
  }

  public static final class TupField extends Field {
    public final int f;

    public TupField(int f) {
      this.f = f;
    }

    @Override
    public boolean equals(Object rhs) {
      return rhs instanceof TupField && f == ((TupField)rhs).f;
    }

    @Override
    public String toString() {
      return String.valueOf(f);
    }

    @Override
    public int compareTo(Field rhs) {
      if (!(rhs instanceof TupField))
        throw new IllFormedASTException();

      return Integer.compare(f, ((TupField)rhs).f);
    }

    @Override
    public boolean isRecField() {
      return false;
    }
  }

  private Field() { }

  public abstract boolean isRecField();

  public static <A> String showFields(Map<Field, A> m, boolean isRec, boolean isWithExpr, String sep) {
    if (isRec)
      return m.toString(isWithExpr ? "| " : "{ ", " }", sep);

    if (isWithExpr)
        return m.toString("| ", " }", sep);

    var l = m.toList().map(Pair::snd_);
    return l.toString("(", l.length() == 1 ? ",)" : ")");
  }

  public static void checkMember(Type trec, Map<Field, Type> m, Field idx, Type t) {
    m.lookup(idx).matchDo(
      () -> { throw new NoFieldInRecException(trec, idx); },
      t_ -> t.expect(t_));
  }
}
