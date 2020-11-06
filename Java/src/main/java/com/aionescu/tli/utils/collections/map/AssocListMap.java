package com.aionescu.tli.utils.collections.map;

import java.util.function.BinaryOperator;

import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.control.Maybe;

public final class AssocListMap<K extends Comparable<K>, V> implements Map<K, V> {
  private final List<Pair<K, V>> _list;

  private AssocListMap(List<Pair<K, V>> list) {
    _list = list;
  }

  public static <K extends Comparable<K>, V> AssocListMap<K, V> empty() {
    return new AssocListMap<>(List.nil());
  }

  @Override
  public String toString() {
    return toString("{ ", " }", " <- ");
  }

  @Override
  public String toString(String begin, String end, String sep) {
    return _list.map(Pair.match((k, v) -> k + sep + v)).toString(begin, end);
  }

  @Override
  public List<Pair<K, V>> toList() {
    return _list;
  }

  @Override
  public Map<K, V> insert(K k, V v) {
    return new AssocListMap<>(_list.insertSorted((a, b) -> a.fst.compareTo(b.fst), Pair.of(k, v)));
  }

  @Override
  public Maybe<V> lookup(K k) {
    return _list.find(p -> p.fst.compareTo(k) == 0).map(Pair::snd_);
  }

  @Override
  public Map<K, V> intersectWith(Map<K, V> rhs, BinaryOperator<V> f) {
    var r = rhs.toList();
    var l = _list.map(a ->
      r.find(p -> p.fst.compareTo(a.fst) == 0).match(
        () -> a,
        b -> Pair.of(a.fst, f.apply(a.snd, b.snd))));

    return new AssocListMap<>(l);
  }
}
