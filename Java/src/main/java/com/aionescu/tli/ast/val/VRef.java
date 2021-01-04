package com.aionescu.tli.ast.val;

import java.util.function.UnaryOperator;

import com.aionescu.tli.ast.prog.GCStats;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.eval.PanicException;
import com.aionescu.tli.utils.data.set.Set;

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
    return -1;
  }

  @Override
  public Type type() {
    throw new PanicException();
  }

  @Override
  public Set<Integer> getInnerAddrs() {
    return Set.singleton(addr);
  }

  @Override
  public Val mapInnerAddrs(UnaryOperator<Integer> f) {
    return new VRef(f.apply(addr));
  }
}
