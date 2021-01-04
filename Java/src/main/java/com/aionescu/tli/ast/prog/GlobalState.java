package com.aionescu.tli.ast.prog;

import com.aionescu.tli.utils.data.list.List;
import com.aionescu.tli.utils.data.map.Map;
import com.aionescu.tli.utils.data.stack.Stack;
import java.util.concurrent.atomic.AtomicReference;

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

  static AtomicReference<GlobalState> _initial(List<Stmt> stmts) {
    var ref = emptyRef();
    ref.getAndUpdate(g -> g.withThreads(List.singleton(ThreadState.initial(ref, 0).withToDo(Stack.ofList(stmts)))).withNextID(1));

    return ref;
  }

  public static AtomicReference<GlobalState> emptyRef() {
    return new AtomicReference<>(empty);
  }

  public static AtomicReference<GlobalState> initialExploded(Stmt stmt) {
    return _initial(stmt.explode());
  }

  public static AtomicReference<GlobalState> initial(Stmt stmt) {
    return _initial(List.singleton(stmt));
  }

  public static void eval(AtomicReference<GlobalState> global) {
    var oldThreads = global.getAndUpdate(g -> g.withThreads(List.nil())).threads;

    var evaledThreads =
      List.ofStream(
        oldThreads
        .stream()
        .parallel()
        .map(ThreadState::eval))
      .sortBy(t -> t.id);

    global.getAndUpdate(g -> g.withThreads(evaledThreads.append(g.threads).filter(ThreadState::isNotDone)));
    GCStats.runGC(global);
  }

  @Override
  public String toString() {
    return String.format("open = %s\ngcStats = %s\nheap = %s\nout = %s\nthreads = \n%s\n",
      open.toString(VStr::escapeString), gcStats, heap.toString(GCStats::showHex), out.reverse(), ThreadState.showThreads(threads));
  }
}
