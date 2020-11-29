package com.aionescu.tli.ast.prog;

import java.util.function.UnaryOperator;

import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.data.list.List;
import com.aionescu.tli.utils.data.map.Map;
import com.aionescu.tli.utils.data.set.Set;
import com.aionescu.tli.utils.control.Ref;
import com.aionescu.tli.utils.data.Foldable;

public final class GCStats {
  public final int allocsSinceGC, gcThreshold, crrHeapSize, maxHeapSize;

  public static final GCStats empty = new GCStats(0, 64, 0, 512);

  public GCStats(int allocsSinceGC, int gcThreshold, int crrHeapSize, int maxHeapSize) {
    this.allocsSinceGC = allocsSinceGC;
    this.gcThreshold = gcThreshold;
    this.crrHeapSize = crrHeapSize;
    this.maxHeapSize = maxHeapSize;
  }

  @Override
  public String toString() {
    return String.format("{ allocs = %s / %s, heapSize = %s / %s }", allocsSinceGC, gcThreshold, crrHeapSize, maxHeapSize);
  }

  public static String showHex(int value) {
    return String.format("0x%x", value);
  }

  public GCStats withAllocsSinceGC(int allocsSinceGC) {
    return new GCStats(allocsSinceGC, gcThreshold, crrHeapSize, maxHeapSize);
  }

  public GCStats withGCThreshold(int gcThreshold) {
    return new GCStats(allocsSinceGC, gcThreshold, crrHeapSize, maxHeapSize);
  }

  public GCStats withCrrHeapSize(int crrHeapSize) {
    return new GCStats(allocsSinceGC, gcThreshold, crrHeapSize, maxHeapSize);
  }

  public GCStats withMaxHeapSize(int maxHeapSize) {
    return new GCStats(allocsSinceGC, gcThreshold, crrHeapSize, maxHeapSize);
  }

  public static <F extends Foldable<Val>> Set<Integer> getInnerAddrsScope(F scope) {
    return scope.foldL((s, a) -> s.union(a.getInnerAddrs()), Set.empty());
  }

  public static Set<Integer> getInnerAddrsDerefed(Map<Integer, Val> heap, Set<Integer> addrs) {
    return getInnerAddrsScope(addrs.toList().map(a -> heap.lookup(a).unwrap()));
  }

  public static Set<Integer> getInnerAddrsAll(Map<Integer, Val> heap, Set<Integer> acc, Set<Integer> set) {
    if (set.isEmpty())
      return acc;
    else {
      var newAcc = acc.union(set);
      return getInnerAddrsAll(heap, newAcc, getInnerAddrsDerefed(heap, set).diff(newAcc));
    }
  }

  public static <K extends Comparable<K>> Set<Integer> getInnerAddrs(Map<Integer, Val> heap, Set<Integer> addrs) {
    return getInnerAddrsAll(heap, Set.empty(), addrs);
  }

  public static Set<Integer> getInnerAddrsThreads(List<ThreadState> threads) {
    return threads.map(t -> t.sym).map(s -> getInnerAddrsScope(s)).foldL((s, a) -> s.union(a), Set.<Integer>empty());
  }

  public static <K extends Comparable<K>> Map<K, Val> mapInnerAddrsMap(UnaryOperator<Integer> f, Map<K, Val> m) {
    return m.map(a -> a.mapInnerAddrs(f));
  }

  public static UnaryOperator<Integer> compactKeys(List<Integer> addrs) {
    var m = Map.fromList(addrs.zip(List.range(0, addrs.length())));
    return i -> m.lookup(i).unwrap();
  }

  public static void runGC(Ref<GlobalState> global) {
    var prog = global.get();

    var gcStats = prog.gcStats;

    if (gcStats.allocsSinceGC < gcStats.gcThreshold)
      return;

    var heap = prog.heap.restrictKeys(getInnerAddrs(prog.heap, getInnerAddrsThreads(prog.threads)));
    var f = compactKeys(heap.toList().map(Pair::fst_));
    var heapCompacted = mapInnerAddrsMap(f, heap);

    var threadsCompacted = prog.threads.map(t -> t.withSym(mapInnerAddrsMap(f, t.sym)));

    var heapMapped = Map.fromList(heapCompacted.toList().map(p -> Pair.of(f.apply(p.fst), p.snd)));
    var heapSize = heapMapped.toList().length();

    var newProg =
      prog
      .withThreads(threadsCompacted)
      .withHeap(heapMapped)
      .withGCStats(
        gcStats
          .withAllocsSinceGC(0)
          .withCrrHeapSize(heapSize));

    global.set(newProg);
  }
}
