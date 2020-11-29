package com.aionescu.tli.utils.data.set;

import java.util.function.Function;

import com.aionescu.tli.utils.Unit;
import com.aionescu.tli.utils.data.list.List;
import com.aionescu.tli.utils.data.map.Map;

public interface Set<A extends Comparable<A>> {
  static <A extends Comparable<A>> Set<A> empty() {
    return UnitMapSet.empty();
  }

  static <A extends Comparable<A>> Set<A> singleton(A a) {
    return Set.<A>empty().insert(a);
  }

  static <A extends Comparable<A>> Set<A> fromList(List<A> list) {
    return list.foldL((s, a) -> s.insert(a), empty());
  }

  List<A> toList();
  Map<A, Unit> toMap();

  boolean isEmpty();
  Set<A> insert(A a);

  Set<A> diff(Set<A> rhs);
  Set<A> union(Set<A> rhs);

  <B extends Comparable<B>> Set<B> map(Function<A, B> f);

}
