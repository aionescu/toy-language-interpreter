package com.aionescu.tli.utils.collections.map;

import java.util.function.BinaryOperator;
import java.util.stream.Stream;

import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.control.Maybe;

public interface Map<K extends Comparable<K>, V> {
  static <K extends Comparable<K>, V> Map<K, V> empty() {
    return CopyingTreeMap.empty();
  }

  String toString(String begin, String end, String sep);

  Stream<Pair<K, V>> stream();

  Map<K, V> insert(K k, V v);
  Maybe<V> lookup(K k);

  Map<K, V> intersect(Map<K, V> rhs);
  Map<K, V> intersectWith(Map<K, V> rhs, BinaryOperator<V> f);
}
