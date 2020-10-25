package com.aionescu.tli.utils.collections.map;

import com.aionescu.tli.utils.control.Maybe;

public interface Map<K, V> {
  static <K, V> Map<K, V> empty() {
    return CopyingHashMap.empty();
  }

  Map<K, V> insert(K k, V v);
  Maybe<V> lookup(K k);
}
