package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.val.Bool;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.VarInfo;
import com.aionescu.tli.utils.collections.map.Map;

public final class If implements Stmt {
  private final Expr _cond;
  private final Stmt _then, _else;

  public If(Expr cond, Stmt then, Stmt else_) {
    _cond = cond;
    _then = then;
    _else = else_;
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    _cond.typeCheck(sym).expect(Type.BOOL);
    _then.typeCheck(sym);
    _else.typeCheck(sym);
    return sym;
  }

  @Override
  public ProgState eval(ProgState prog) {
    var v = ((Bool)_cond.eval(prog.sym)).val;
    var block = v ? _then : _else;

    return prog.withToDo(prog.toDo.push(block));
  }

  @Override
  public String toString() {
    return String.format("if %s { %s } else { %s }", _cond, _then, _else);
  }
}
