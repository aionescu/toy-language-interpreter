package com.aionescu.tli.utils.data.map;

import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;

import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.TriFunction;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.data.Foldable;
import com.aionescu.tli.utils.data.list.List;
import com.aionescu.tli.utils.data.set.Set;

public interface Map<K extends Comparable<K>, V> extends Foldable<V> {
  static <K extends Comparable<K>, V> Map<K, V> empty() {
    return AssocListMap.empty();
  }

  static <K extends Comparable<K>, V> Map<K, V> fromList(List<Pair<K, V>> list) {
    return list.foldL((s, a) -> s.insert(a.fst, a.snd), empty());
  }

  String toString(String begin, String end, String sep);
  String toString(Function<K, String> keyFmt);
  List<Pair<K, V>> toList();
  int length();

  Map<K, V> insert(K k, V v);
  Map<K, V> delete(K k);
  Maybe<V> lookup(K k);

  Map<K, V> restrictKeys(Set<K> keys);

  Map<K, V> diff(Map<K, V> rhs);
  Map<K, V> intersectWith(Map<K, V> rhs, BinaryOperator<V> f);
  Map<K, V> unionWith(Map<K, V> rhs, BinaryOperator<V> f);

  <W> Map<K, W> map(Function<V, W> f);
  <W> Map<K, W> mapWithKey(BiFunction<K, V, W> f);

  <S> S foldLWithKey(TriFunction<S, K, V, S> f, S z);

  default Map<K, V> intersect(Map<K, V> rhs) {
    return intersectWith(rhs, (a, b) -> a);
  }

  default Map<K, V> union(Map<K, V> rhs) {
    return unionWith(rhs, (a, b) -> a);
  }

  static <K extends Comparable<K>, V extends Comparable<V>> int compare(Map<K, V> a, Map<K, V> b) {
    List<Pair<K, V>> as = a.toList();
    List<Pair<K, V>> bs = b.toList();

    List<K> asFst = as.map(p -> p.fst);
    List<K> bsFst = bs.map(p -> p.fst);

    var keyOrd = List.compare(asFst, bsFst);

    List<V> asSnd = as.map(p -> p.snd);
    List<V> bsSnd = bs.map(p -> p.snd);

    return
      keyOrd != 0
      ? keyOrd
      : List.compare(asSnd, bsSnd);
  }
}
