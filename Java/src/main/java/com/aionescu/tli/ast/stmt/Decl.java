package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.VarInfo;
import com.aionescu.tli.ast.type.VarState;
import com.aionescu.tli.exn.typeck.VariableAlreadyDeclaredException;

public final class Decl implements Stmt {
  private final Ident _ident;
  private final Type _type;

  public static Decl of(Ident ident, Type type) {
    return new Decl(ident, type);
  }

  public Decl(Ident ident, Type type) {
    _ident = ident;
    _type = type;
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    return sym.lookup(_ident).match(
      () -> sym.insert(_ident, VarInfo.of(_type, VarState.UNINIT)),
      a -> { throw new VariableAlreadyDeclaredException(_ident); });
  }

  @Override
  public ProgState eval(ProgState prog) {
    return prog;
  }

  @Override
  public String toString() {
    return String.format("%s : %s", _ident, _type);
  }
}
