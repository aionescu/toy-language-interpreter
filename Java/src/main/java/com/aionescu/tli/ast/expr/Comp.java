package com.aionescu.tli.ast.expr;

import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VBool;
import com.aionescu.tli.ast.val.Val;

public final class Comp implements Expr {
  public static enum Op {
    LT,
    LTE,
    GT,
    GTE,
    EQ,
    NEQ;

    @Override
    public String toString() {
      return switch (this) {
        case LT -> "<";
        case LTE -> "<=";
        case GT -> ">";
        case GTE -> ">=";
        case EQ -> "==";
        case NEQ -> "!=";
      };
    }
  }

  private final Expr _lhs, _rhs;
  private final Op _op;

  public Comp(Expr lhs, Op op, Expr rhs) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    _rhs.typeCheck(sym).mustBe(_lhs.typeCheck(sym));
    return TBool.t;
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {

    var lhs = _lhs.eval(sym);
    var rhs = _rhs.eval(sym);
    var ordering = lhs.compareTo(rhs);

    return new VBool(switch (_op) {
      case LT -> ordering < 0;
      case LTE -> ordering <= 0;
      case GT -> ordering > 0;
      case GTE -> ordering >= 0;
      case EQ -> ordering == 0;
      case NEQ -> ordering != 0;
    });
  }

  @Override
  public String toString() {
    return String.format("(%s %s %s)", _lhs, _op, _rhs);
  }
}
