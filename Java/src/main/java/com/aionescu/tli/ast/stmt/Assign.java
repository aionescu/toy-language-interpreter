package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Field;
import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.expr.RecMember;
import com.aionescu.tli.ast.expr.RecWith;
import com.aionescu.tli.ast.expr.Var;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.type.varinfo.VarState;
import com.aionescu.tli.exn.typeck.InvalidAssignmentException;
import com.aionescu.tli.exn.typeck.UndeclaredVariableException;
import com.aionescu.tli.utils.collections.map.Map;

public final class Assign implements Stmt {
  private final Expr _lhs;
  private final Expr _rhs;

  public Assign(Expr lhs, Expr rhs) {
    _lhs = lhs;
    _rhs = rhs;
  }

  @Override
  public String toString() {
    return String.format("%s <- %s", _lhs, _rhs);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    if (_lhs instanceof Var) {
      var ident = ((Var)_lhs).ident;

      var info = sym.lookup(ident).match(
        () -> { throw new UndeclaredVariableException(ident); },
        a -> a);

      _rhs.typeCheck(sym).mustBe(info.type);
      return sym.insert(ident, new VarInfo(info.type, VarState.INIT));
    } else if (_lhs instanceof RecMember) {
      var lhs = (RecMember)_lhs;
      return new Assign(lhs.lhs, new RecWith(lhs.lhs, lhs.field.isRecField(), Map.<Field, Expr>empty().insert(lhs.field, _rhs))).typeCheck(sym);
    } else
      throw new InvalidAssignmentException();
  }

  @Override
  public ProgState eval(ProgState prog) {
    if (_lhs instanceof Var) {
      var ident = ((Var)_lhs).ident;
      return prog.withSym(prog.sym.insert(ident, _rhs.eval(prog.sym)));
    } else {
      var lhs = (RecMember)_lhs;
      return new Assign(lhs.lhs, new RecWith(lhs.lhs, lhs.field.isRecField(), Map.<Field, Expr>empty().insert(lhs.field, _rhs))).eval(prog);
    }
  }
}
