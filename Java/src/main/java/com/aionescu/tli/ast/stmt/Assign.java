package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.Field.FRec;
import com.aionescu.tli.ast.Field.FTup;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.expr.RecMember;
import com.aionescu.tli.ast.expr.RecWith;
import com.aionescu.tli.ast.expr.Var;
import com.aionescu.tli.ast.expr.kind.ExprKind.L;
import com.aionescu.tli.ast.expr.kind.ExprKind.R;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.type.varinfo.VarState;
import com.aionescu.tli.exn.typeck.UndeclaredVariableException;
import com.aionescu.tli.utils.collections.map.Map;

public final class Assign implements Stmt {
  private final Expr<L> _lhs;
  private final Expr<R> _rhs;

  public Assign(Expr<L> lhs, Expr<R> rhs) {
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
      var ident = ((Var<L>)_lhs).ident;

      var info = sym.lookup(ident).match(
        () -> { throw new UndeclaredVariableException(ident); },
        a -> a);

      _rhs.typeCheck(sym).expect(info.type);
      return sym.insert(ident, new VarInfo(info.type, VarState.INIT));
    } else if (_lhs instanceof RecMember<?, ?, ?>) {
      @SuppressWarnings("unchecked")
      var lhs = (RecMember<?, ?, L>)_lhs;

      if (lhs.f instanceof FRec)
        return new Assign(lhs.lhs, new RecWith<>(lhs.lhs, (FRec)lhs.f, Map.<Ident, Expr<?>>empty().insert((Ident)lhs.idx, _rhs))).typeCheck(sym);
      else if (lhs.f instanceof FTup)
        return new Assign(lhs.lhs, new RecWith<>(lhs.lhs, (FTup)lhs.f, Map.<Integer, Expr<?>>empty().insert((Integer)lhs.idx, _rhs))).typeCheck(sym);
      else
        throw new IllegalStateException("Is there a 3rd record kind?");
    } else
      throw new IllegalStateException("Invalid LHS in assignment. Did you run the typechecker?");
  }

  @Override
  public ProgState eval(ProgState prog) {
    if (_lhs instanceof Var) {
      var ident = ((Var<L>)_lhs).ident;

      return prog.withSym(prog.sym.insert(ident, _rhs.eval(prog.sym)));
    } else if (_lhs instanceof RecMember<?, ?, ?>) {
      @SuppressWarnings("unchecked")
      var lhs = (RecMember<?, ?, L>)_lhs;

      if (lhs.f instanceof FRec)
        return new Assign(lhs.lhs, new RecWith<>(lhs.lhs, (FRec)lhs.f, Map.<Ident, Expr<?>>empty().insert((Ident)lhs.idx, _rhs))).eval(prog);
      else if (lhs.f instanceof FTup)
        return new Assign(lhs.lhs, new RecWith<>(lhs.lhs, (FTup)lhs.f, Map.<Integer, Expr<?>>empty().insert((Integer)lhs.idx, _rhs))).eval(prog);
      else
        throw new IllegalStateException("Is there a 3rd record kind?");
    } else
      throw new IllegalStateException("Invalid LHS in assignment. Did you run the typechecker?");
  }
}
