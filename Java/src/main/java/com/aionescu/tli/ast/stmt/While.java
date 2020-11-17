package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.val.VBool;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.TBool;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.utils.collections.map.Map;

public final class While implements Stmt {
  private final Expr _cond;
  private final Stmt _body;

  public While(Expr cond, Stmt body) {
    _cond = cond;
    _body = body;
  }

  @Override
  public String toString() {
    return String.format("while %s { %s }", _cond, _body);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    _cond.typeCheck(sym).mustBe(TBool.t);
    _body.typeCheck(sym);
    return sym;
  }

  @Override
  public ProgState eval(ProgState prog) {
    var v = ((VBool)_cond.eval(prog.heap, prog.sym)).val;
    var toDo = v ? prog.toDo.push(this).push(_body) : prog.toDo;

    return prog.withToDo(toDo);
  }
}
