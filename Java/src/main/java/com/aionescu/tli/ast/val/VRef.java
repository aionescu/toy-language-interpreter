package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.prog.GCStats;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.eval.PanicException;

public final class VRef extends Val {
  public final int addr;

  public VRef(int addr) {
    this.addr = addr;
  }

  @Override
  public String toString() {
    return GCStats.showHex(addr);
  }

  @Override
  public int compareTo(Val arg0) {
    throw new PanicException();
  }

  @Override
  public Type type() {
    throw new PanicException();
  }
}
