package com.aionescu.tli.ast.val;

import java.util.function.UnaryOperator;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.GCStats;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.eval.PanicException;
import com.aionescu.tli.utils.TriFunction;
import com.aionescu.tli.utils.data.map.Map;
import com.aionescu.tli.utils.data.set.Set;

public final class VFun extends Val {
  public final Map<Ident, Val> sym;
  public final TriFunction<Map<Integer, Val>, Map<Ident, Val>, Val, Val> f;

  public VFun(Map<Ident, Val> sym, TriFunction<Map<Integer, Val>, Map<Ident, Val>, Val, Val> f) {
    this.sym = sym;
    this.f = f;
  }

  @Override
  public String toString() {
    return "<λ>";
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
    return GCStats.getInnerAddrsScope(sym);
  }

  @Override
  public Val mapInnerAddrs(UnaryOperator<Integer> f) {
    return new VFun(GCStats.mapInnerAddrsMap(f, sym), this.f);
  }
}
