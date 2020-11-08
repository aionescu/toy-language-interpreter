package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.TRec;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VRec;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.ExpectedRecFoundException;
import com.aionescu.tli.exn.typeck.NoFieldInRecException;
import com.aionescu.tli.utils.collections.map.Map;

public final class RecMember implements Expr {
  public final Expr lhs;
  public final Field field;

  public RecMember(Expr lhs, Field field) {
    this.lhs = lhs;
    this.field = field;
  }

  @Override
  public String toString() {
    return String.format("%s.%s", lhs, field);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    var t = lhs.typeCheck(sym);

    if (!(t instanceof TRec))
      throw new ExpectedRecFoundException(t);

    var trec = (TRec)t;

    if (field.isRecField() != trec.isRec)
      throw new ExpectedRecFoundException(t);

    var result = trec.fields.lookup(field);
    return result.match(() -> { throw new NoFieldInRecException(t, field); }, a -> a);
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    var v = (VRec)lhs.eval(sym);
    return v.fields.lookup(field).unwrap();
  }
}
