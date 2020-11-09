package com.aionescu.tli.ast.expr;

import com.aionescu.tli.utils.collections.map.Map;

import java.math.BigInteger;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.TInt;
import com.aionescu.tli.ast.type.TStr;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VInt;
import com.aionescu.tli.ast.val.VStr;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.eval.DivisionByZeroException;
import com.aionescu.tli.exn.typeck.CanOnlyAddIntegersException;
import com.aionescu.tli.exn.typeck.CanOnlyAppendStringsException;

public final class Arith implements Expr {
  public static enum Op {
    ADD,
    SUB,
    MUL,
    DIV,
    REM;

    @Override
    public String toString() {
      return switch (this) {
        case ADD -> "+";
        case SUB -> "-";
        case MUL -> "*";
        case DIV -> "/";
        case REM -> "%";
      };
    }
  }

  private final Expr _lhs, _rhs;
  private final Op _op;

  public Arith(Expr lhs, Op op, Expr rhs) {
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
    var lhs = _lhs.typeCheck(sym);
    var rhs = _rhs.typeCheck(sym);

    if (_op == Op.ADD) {
      var lhsStr = lhs.equals(TStr.t);
      var rhsStr = rhs.equals(TStr.t);

      var lhsInt = lhs.equals(TInt.t);
      var rhsInt = rhs.equals(TInt.t);

      if (lhsStr || rhsStr) {
        if (!(lhsStr && rhsStr))
          throw new CanOnlyAppendStringsException();

        return TStr.t;
      }

      if (!lhsInt || !rhsInt)
        throw new CanOnlyAddIntegersException();

      return lhsStr ? TStr.t : TInt.t;
    }

    _lhs.typeCheck(sym).mustBe(TInt.t);
    _rhs.typeCheck(sym).mustBe(TInt.t);
    return TInt.t;
  }

  @Override
  public Val eval(Map<Ident, Val> sym) {
    var lhsv = _lhs.eval(sym);
    var rhsv = _rhs.eval(sym);

    if (_op == Op.ADD && lhsv instanceof VStr) {
      return new VStr(((VStr)lhsv).val + ((VStr)rhsv).val);
    }

    var lhs = ((VInt)lhsv).val;
    var rhs = ((VInt)rhsv).val;

    return new VInt(switch (_op) {
      case ADD -> lhs.add(rhs);
      case SUB -> lhs.subtract(rhs);
      case MUL -> lhs.multiply(rhs);
      case DIV -> switch (rhs.compareTo(BigInteger.ZERO)) {
        case 0 -> throw new DivisionByZeroException();
        default -> lhs.divide(rhs);
      };
      case REM -> switch (rhs.compareTo(BigInteger.ZERO)) {
        case 0 -> throw new DivisionByZeroException();
        default -> lhs.remainder(rhs);
      };
    });
  }
}
