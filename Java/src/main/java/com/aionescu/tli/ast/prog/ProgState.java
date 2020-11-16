package com.aionescu.tli.ast.prog;

import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.collections.map.Map;
import com.aionescu.tli.utils.collections.stack.Stack;
import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.stmt.Stmt;
import com.aionescu.tli.ast.val.VStr;
import com.aionescu.tli.ast.val.Val;

public final class ProgState {
  public static final ProgState empty = new ProgState(Map.empty(), Stack.empty(), Map.empty(), GCStats.empty, Map.empty(), List.nil());

  public final Map<String, List<Val>> open;
  public final Stack<Stmt> toDo;
  public final Map<Ident, Val> sym;
  public final GCStats gcStats;
  public final Map<Integer, Val> heap;
  public final List<Val> out;

  private ProgState(Map<String, List<Val>> open, Stack<Stmt> toDo, Map<Ident, Val> sym, GCStats gcStats, Map<Integer, Val> heap, List<Val> out) {
    this.open = open;
    this.toDo = toDo;
    this.sym = sym;
    this.gcStats = gcStats;
    this.heap = heap;
    this.out = out;
  }

  public ProgState withOpen(Map<String, List<Val>> open) {
    return new ProgState(open, toDo, sym, gcStats, heap, out);
  }

  public ProgState withToDo(Stack<Stmt> toDo) {
    return new ProgState(open, toDo, sym, gcStats, heap, out);
  }

  public ProgState withSym(Map<Ident, Val> sym) {
    return new ProgState(open, toDo, sym, gcStats, heap, out);
  }

  public ProgState withGCStats(GCStats gcStats) {
    return new ProgState(open, toDo, sym, gcStats, heap, out);
  }

  public ProgState withOut(List<Val> out) {
    return new ProgState(open, toDo, sym, gcStats, heap, out);
  }

  public String output() {
    return out.reverse().unlines();
  }

  @Override
  public String toString() {
    return String.format("open = %s\ntoDo = %s\nsym = %s\ngcStats = %s\nheap = %s\nout = %s\n",
      open.toString(VStr::escapeString), toDo, sym, gcStats, heap.toString(GCStats::showHex), out.reverse());
  }
}
