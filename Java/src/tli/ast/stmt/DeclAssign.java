package tli.ast.stmt;

import utils.collections.list.List;
import utils.collections.map.Map;

import java.util.Optional;

import tli.ast.Ident;
import tli.ast.expr.Expr;
import tli.ast.prog.ProgState;
import tli.ast.type.Type;
import tli.ast.type.VarInfo;

public final class DeclAssign implements Stmt {
  private final Ident _ident;
  private final Optional<Type> _type;
  private final Expr _expr;

  public static DeclAssign of(Ident ident, Optional<Type> type, Expr expr) {
    return new DeclAssign(ident, type, expr);
  }

  public DeclAssign(Ident ident, Optional<Type> type, Expr expr) {
    _ident = ident;
    _type = type;
    _expr = expr;
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    var type = _type.orElseGet(() -> _expr.typeCheck(sym));

    var sym2 = Decl.of(_ident, type).typeCheck(sym);
    return Assign.of(_ident, _expr).typeCheck(sym2);
  }

  @Override
  public ProgState eval(ProgState prog) {
    var tail = List.cons(Assign.of(_ident, _expr), prog.toDo);

    var newToDo =
      _type.isPresent()
        ? List.cons(Decl.of(_ident, _type.get()), tail)
        : tail;

    return prog.withToDo(newToDo);
  }

  @Override
  public String toString() {
    return String.format("%s : %s <- %s", _ident, _type.map(Object::toString).orElse("_"), _expr);
  }
}
