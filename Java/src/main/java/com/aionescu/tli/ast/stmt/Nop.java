package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.utils.data.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.ThreadState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;

public final class Nop implements Stmt {
  public static final Nop nop = new Nop();

  private Nop() { }

  @Override
  public String toString() {
    return "{- nop -}";
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    return sym;
  }

  @Override
  public ThreadState eval(ThreadState prog) {
    return prog;
  }
}
