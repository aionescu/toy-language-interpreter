package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.utils.collections.map.Map;
import com.aionescu.tli.utils.control.Maybe;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.VarInfo;

public final class DeclAssign implements Stmt {
  private final Ident _ident;
  private final Maybe<Type> _type;
  private final Expr _expr;

  public DeclAssign(Ident ident, Maybe<Type> type, Expr expr) {
    _ident = ident;
    _type = type;
    _expr = expr;
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    var type = _type.match(() -> _expr.typeCheck(sym), a -> a);

    var sym2 = new Decl(_ident, type).typeCheck(sym);
    return new Assign(_ident, _expr).typeCheck(sym2);
  }

  @Override
  public ProgState eval(ProgState prog) {
    var tail = prog.toDo.push(new Assign(_ident, _expr));

    return prog.withToDo(_type.match(
      () -> tail,
      t -> tail.push(new Decl(_ident, t))));
  }

  @Override
  public String toString() {
    return String.format("%s : %s <- %s", _ident, _type.match(() -> "_", Object::toString), _expr);
  }
}
