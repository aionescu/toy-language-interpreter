package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.val.Bool;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.VarInfo;
import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.collections.map.Map;

public final class While implements Stmt {
  private final Expr _cond;
  private final Stmt _body;

  public static While of(Expr cond, Stmt body) {
    return new While(cond, body);
  }

  public While(Expr cond, Stmt body) {
    _cond = cond;
    _body = body;
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    _cond.typeCheck(sym).expect(Type.BOOL);
    _body.typeCheck(sym);
    return sym;
  }

  @Override
  public ProgState eval(ProgState prog) {
    var v = ((Bool)_cond.eval(prog.sym)).val;
    var toDo = v ? List.cons(_body, List.cons(this, prog.toDo)) : prog.toDo;

    return prog.withToDo(toDo);
  }

  @Override
  public String toString() {
    return String.format("while %s { %s }", _cond, _body);
  }
}
