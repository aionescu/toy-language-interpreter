package com.aionescu.tli.utils.collections.map;

import java.util.TreeMap;

import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.control.Maybe;

public final class CopyingTreeMap<K extends Comparable<K>, V> implements Map<K, V> {
  private final TreeMap<K, V> _tm;

  private CopyingTreeMap(TreeMap<K, V> tm) {
    _tm = tm;
  }

  public static <K extends Comparable<K>, V> Map<K, V> empty() {
    return new CopyingTreeMap<K, V>(new TreeMap<>(Comparable::compareTo));
  }

  @Override
  public String toString() {
    return toString("{ ", " }", " <- ");
  }

  @Override
  public String toString(String begin, String end, String sep) {
    var entries = List.ofStream(_tm.entrySet().stream());

    return
      entries
      .map(e -> e.getKey() + sep + e.getValue())
      .toString(begin, end);
  }

  @Override
  public CopyingTreeMap<K, V> insert(K k, V v) {
    var hm = new TreeMap<>(_tm);
    hm.put(k, v);

    return new CopyingTreeMap<>(hm);
  }

  @Override
  public Maybe<V> lookup(K k) {
    return _tm.containsKey(k) ? Maybe.just(_tm.get(k)) : Maybe.nothing();
  }
}
