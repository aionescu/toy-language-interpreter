package com.aionescu.tli.ast.expr;

import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.VarInfo;
import com.aionescu.tli.ast.val.Bool;
import com.aionescu.tli.ast.val.Val;

public final class Logic implements Expr {
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

  private final Expr _lhs, _rhs;
  private final Op _op;

  public Logic(Expr lhs, Op op, Expr rhs) {
    _lhs = lhs;
    _rhs = rhs;
    _op = op;
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    _lhs.typeCheck(sym).expect(Type.BOOL);
    _rhs.typeCheck(sym).expect(Type.BOOL);
    return Type.BOOL;
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    var lhs = ((Bool)_lhs.eval(sym)).val;

    if (lhs == false && _op == Op.AND)
      return new Bool(false);

    if (lhs == true && _op == Op.OR)
      return new Bool(true);

    var rhs = ((Bool)_rhs.eval(sym)).val;

    return new Bool(switch (_op) {
      case AND -> lhs && rhs;
      case OR -> lhs || rhs;
    });
  }

  @Override
  public String toString() {
    return String.format("(%s %s %s)", _lhs, _op, _rhs);
  }
}
