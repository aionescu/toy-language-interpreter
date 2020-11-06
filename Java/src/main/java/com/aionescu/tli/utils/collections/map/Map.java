package com.aionescu.tli.utils.collections.map;

import java.util.function.BinaryOperator;
import java.util.function.Function;

import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.control.Maybe;

public interface Map<K extends Comparable<K>, V> {
  static <K extends Comparable<K>, V> Map<K, V> empty() {
    return AssocListMap.empty();
  }

  String toString(String begin, String end, String sep);
  List<Pair<K, V>> toList();

  Map<K, V> insert(K k, V v);
  Maybe<V> lookup(K k);

  Map<K, V> intersectWith(Map<K, V> rhs, BinaryOperator<V> f);

  <W> Map<K, W> map(Function<V, W> f);

  default Map<K, V> intersect(Map<K, V> rhs) {
    return intersectWith(rhs, (a, b) -> a);
  }

  static <K extends Comparable<K>, V extends Comparable<V>> int compare(Map<K, V> a, Map<K, V> b) {
    var as = a.toList();
    var bs = b.toList();

    var keyOrd = List.compare(as.map(Pair::fst_), bs.map(Pair::fst_));

    return
      keyOrd != 0
      ? keyOrd
      : List.compare(as.map(Pair::snd_), bs.map(Pair::snd_));
  }
}
