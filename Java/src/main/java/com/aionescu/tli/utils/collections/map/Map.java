package com.aionescu.tli.utils.collections.map;

import com.aionescu.tli.utils.control.Maybe;

public interface Map<K extends Comparable<K>, V> {
  static <K extends Comparable<K>, V> Map<K, V> empty() {
    return CopyingTreeMap.empty();
  }

  String toString(String begin, String end, String sep);

  Map<K, V> insert(K k, V v);
  Maybe<V> lookup(K k);
}
