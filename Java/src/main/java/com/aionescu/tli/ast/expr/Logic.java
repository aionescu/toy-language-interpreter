package com.aionescu.tli.ast.expr;

import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.kind.ExprKind.R;
import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VBool;
import com.aionescu.tli.ast.val.Val;

public final class Logic implements Expr<R> {
  public static enum Op {
    AND,
    OR;

    @Override
    public String toString() {
      return switch (this) {
        case AND -> "and";
        case OR -> "or";
      };
    }
  }

  private final Expr<?> _lhs, _rhs;
  private final Op _op;

  public Logic(Expr<?> lhs, Op op, Expr<?> rhs) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
  }

  @Override
  public String toString() {
    return String.format("(%s %s %s)", _lhs, _op, _rhs);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    _lhs.typeCheck(sym).expect(TBool.t);
    _rhs.typeCheck(sym).expect(TBool.t);
    return TBool.t;
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    var lhs = ((VBool)_lhs.eval(sym)).val;

    if (lhs == false && _op == Op.AND)
      return new VBool(false);

    if (lhs == true && _op == Op.OR)
      return new VBool(true);

    var rhs = ((VBool)_rhs.eval(sym)).val;

    return new VBool(switch (_op) {
      case AND -> lhs && rhs;
      case OR -> lhs || rhs;
    });
  }
}
