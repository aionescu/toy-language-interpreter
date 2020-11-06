package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.expr.kind.ExprKind.R;
import com.aionescu.tli.ast.val.VBool;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.type.varinfo.VarState;
import com.aionescu.tli.utils.collections.map.Map;

public final class If implements Stmt {
  private final Expr<R> _cond;
  private final Stmt _then, _else;

  public If(Expr<R> cond, Stmt then, Stmt else_) {
    _cond = cond;
    _then = then;
    _else = else_;
  }

  @Override
  public String toString() {
    return String.format("if %s { %s } else { %s }", _cond, _then, _else);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    _cond.typeCheck(sym).expect(TBool.t);
    var symT = _then.typeCheck(sym);
    var symE = _else.typeCheck(sym);

    var symIf = symT.intersectWith(symE, (a, b) ->
      a.state == b.state
      ? a
      : new VarInfo(a.type, VarState.UNINIT));

    return symIf.intersect(sym);
  }

  @Override
  public ProgState eval(ProgState prog) {
    var v = ((VBool)_cond.eval(prog.sym)).val;
    var block = v ? _then : _else;

    return prog.withToDo(prog.toDo.push(block));
  }
}
