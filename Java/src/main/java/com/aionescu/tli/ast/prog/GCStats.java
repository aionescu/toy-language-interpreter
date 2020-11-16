package com.aionescu.tli.ast.prog;

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
}
