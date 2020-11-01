package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.VarInfo;

public final class Nop implements Stmt {
  @Override
  public String toString() {
    return "";
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    return sym;
  }

  @Override
  public ProgState eval(ProgState prog) {
    return prog;
  }
}
