package com.aionescu.tli.ast.prog;

import com.aionescu.tli.exn.eval.InexistentBarrierException;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.data.map.Map;
import com.aionescu.tli.utils.data.set.Set;

public final class BarrierTableImpl implements BarrierTable {
  public static final BarrierTableImpl empty = new BarrierTableImpl(Map.empty());

  private final Map<Integer, Pair<Integer, Set<Integer>>> _map;

  private BarrierTableImpl(Map<Integer, Pair<Integer, Set<Integer>>> map) {
    _map = map;
  }

  @Override
  public String toString() {
    return _map.toString();
  }

  @Override
  public int nextFreeLocation() {
    return _map.length();
  }

  @Override
  public Maybe<Pair<Integer, Set<Integer>>> lookup(int location) {
    return _map.lookup(location);
  }

  @Override
  public BarrierTable addBarrier(int count) {
    return new BarrierTableImpl(_map.insert(nextFreeLocation(), Pair.of(count, Set.empty())));
  }

  @Override
  public BarrierTable addThread(int location, int threadID) {
    var p = lookup(location).unwrap(InexistentBarrierException::new);
    var set = p.snd.insert(threadID);

    return new BarrierTableImpl(_map.insert(location, Pair.of(p.fst, set)));
  }

  @Override
  public Map<Integer, Pair<Integer, Set<Integer>>> toMap() {
    return _map;
  }
}
