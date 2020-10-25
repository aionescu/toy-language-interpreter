package com.aionescu.tli.utils.collections.map;

import java.util.HashMap;

import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.control.Maybe;

public final class CopyingHashMap<K, V> implements Map<K, V> {
  private final HashMap<K, V> _hm;

  public static <K, V> Map<K, V> empty() {
    return new CopyingHashMap<>(new HashMap<>());
  }

  private CopyingHashMap(HashMap<K, V> hm) {
    _hm = hm;
  }

  @Override
  public CopyingHashMap<K, V> insert(K k, V v) {
    var hm = new HashMap<>(_hm);
    hm.put(k, v);

    return new CopyingHashMap<>(hm);
  }

  @Override
  public Maybe<V> lookup(K k) {
    return _hm.containsKey(k) ? Maybe.just(_hm.get(k)) : Maybe.nothing();
  }

  @Override
  public String toString() {
    var entries = List.ofStream(_hm.entrySet().stream());

    return
      entries
      .map(e -> e.getKey() + " <- " + e.getValue())
      .toString()
      .replace("[]", "{ }")
      .replace("[", "{ ")
      .replace("]", " }");
  }
}
