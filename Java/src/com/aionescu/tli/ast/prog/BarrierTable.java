package com.aionescu.tli.ast.prog;

import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.data.map.Map;
import com.aionescu.tli.utils.data.set.Set;

public interface BarrierTable {
  int nextFreeLocation();
  Maybe<Pair<Integer, Set<Integer>>> lookup(int location);
  BarrierTable addBarrier(int count);
  BarrierTable addThread(int location, int threadID);
  Map<Integer, Pair<Integer, Set<Integer>>> toMap();
}
