package com.aionescu.tli.ast.prog;

import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.collections.map.Map;
import com.aionescu.tli.utils.collections.stack.Stack;
import com.aionescu.tli.utils.control.Ref;
import com.aionescu.tli.ast.stmt.Stmt;
import com.aionescu.tli.ast.val.VStr;
import com.aionescu.tli.ast.val.Val;

public final class GlobalState {
  public static final GlobalState empty = new GlobalState(Map.empty(), GCStats.empty, Map.empty(), List.nil(), List.nil(), 0);

  public final Map<String, List<Val>> open;
  public final GCStats gcStats;
  public final Map<Integer, Val> heap;
  public final List<Val> out;
  public final List<ThreadState> threads;
  public final int nextID;

  private GlobalState(Map<String, List<Val>> open, GCStats gcStats, Map<Integer, Val> heap, List<Val> out, List<ThreadState> threads, int nextID) {
    this.open = open;
    this.gcStats = gcStats;
    this.heap = heap;
    this.out = out;
    this.threads = threads;
    this.nextID = nextID;
  }

  public GlobalState withOpen(Map<String, List<Val>> open) {
    return new GlobalState(open, gcStats, heap, out, threads, nextID);
  }

  public GlobalState withGCStats(GCStats gcStats) {
    return new GlobalState(open, gcStats, heap, out, threads, nextID);
  }

  public GlobalState withHeap(Map<Integer, Val> heap) {
    return new GlobalState(open, gcStats, heap, out, threads, nextID);
  }

  public GlobalState withOut(List<Val> out) {
    return new GlobalState(open, gcStats, heap, out, threads, nextID);
  }

  public GlobalState withThreads(List<ThreadState> threads) {
    return new GlobalState(open, gcStats, heap, out, threads, nextID);
  }

  public GlobalState withNextID(int nextID) {
    return new GlobalState(open, gcStats, heap, out, threads, nextID);
  }

  public String output() {
    return out.reverse().unlines();
  }

  public static Ref<GlobalState> initial(Stmt stmt) {
    var ref = new Ref<GlobalState>(GlobalState.empty);
    ref.update(g -> g.withThreads(List.singleton(ThreadState.initial(ref, 0).withToDo(Stack.of(stmt)))).withNextID(1));

    return ref;
  }

  public static void eval(Ref<GlobalState> global) {
    var threads =
      List.ofStream(
        global.get().threads
        .stream()
        .parallel()
        .map(ThreadState::eval));

    var newThreads = global.get().threads.filter(t -> !threads.any(t1 -> t1.id == t.id));

    global.update(g -> g.withThreads(threads.append(newThreads).filter(ThreadState::isNotDone).sortBy(t -> t.id)));
    GCStats.runGC(global);
  }

  @Override
  public String toString() {
    return String.format("open = %s\ngcStats = %s\nheap = %s\nout = %s\nthreads = \n%s\n",
      open.toString(VStr::escapeString), gcStats, heap.toString(GCStats::showHex), out.reverse(), ThreadState.showThreads(threads));
  }
}
