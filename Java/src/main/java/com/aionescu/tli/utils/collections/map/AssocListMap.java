package com.aionescu.tli.utils.collections.map;

import java.util.Comparator;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;

import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.TriFunction;
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
  public boolean equals(Object rhs) {
    return rhs instanceof Map<?, ?> && _list.equals((((Map<?, ?>)rhs).toList()));
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
  public <W> Map<K, W> map(Function<V, W> f) {
    return new AssocListMap<>(_list.map(Pair.match((k, v) -> Pair.of(k, f.apply(v)))));
  }

  @Override
  public <W> Map<K, W> mapWithKey(BiFunction<K, V, W> f) {
    return new AssocListMap<>(_list.map(p -> Pair.of(p.fst, f.apply(p.fst, p.snd))));
  }

  @Override
  public <S> S foldlWithKey(TriFunction<S, K, V, S> f, S z) {
    return _list.foldl((s, p) -> f.apply(s, p.fst, p.snd), z);
  }

  @Override
  public Map<K, V> diff(Map<K, V> rhs) {
    var l = rhs.toList();
    return new AssocListMap<>(_list.filter(p -> l.find(p_ -> p.fst.compareTo(p_.fst) == 0).match(() -> true, j_ -> false)));
  }

  @Override
  public Map<K, V> intersectWith(Map<K, V> rhs, BinaryOperator<V> f) {
    var r = rhs.toList();
    var l = _list.bind(a ->
      r.find(p -> p.fst.compareTo(a.fst) == 0).match(
        () -> List.nil(),
        b -> List.singleton(Pair.of(a.fst, f.apply(a.snd, b.snd)))));

    return new AssocListMap<>(l);
  }

  @Override
  public Map<K, V> unionWith(Map<K, V> rhs, BinaryOperator<V> f) {
    var these = intersectWith(rhs, f).toList();
    var this_ = diff(rhs).toList();
    var that_ = rhs.diff(this).toList();
    Comparator<Pair<K, V>> c = (a, b) -> a.fst.compareTo(b.fst);

    return new AssocListMap<>(this_.mergeSorted(c, that_).mergeSorted(c, these));
  }
}
