package com.aionescu.tli.utils.collections.set;

import java.util.function.Function;

import com.aionescu.tli.utils.Unit;
import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.collections.map.Map;

public interface Set<A extends Comparable<A>> {
  static <A extends Comparable<A>> Set<A> empty() {
    return UnitMapSet.empty();
  }

  static <A extends Comparable<A>> Set<A> singleton(A a) {
    return Set.<A>empty().insert(a);
  }

  List<A> toList();
  Map<A, Unit> toMap();

  boolean isEmpty();
  Set<A> insert(A a);

  Set<A> diff(Set<A> rhs);
  Set<A> union(Set<A> rhs);

  <B extends Comparable<B>> Set<B> map(Function<A, B> f);

}
