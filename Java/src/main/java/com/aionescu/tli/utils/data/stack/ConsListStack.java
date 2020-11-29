package com.aionescu.tli.utils.data.stack;

import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.data.list.List;
import com.aionescu.tli.utils.control.Maybe;

public final class ConsListStack<A> implements Stack<A> {
  private final List<A> _list;

  private ConsListStack(List<A> list) {
    _list = list;
  }

  public static <A> ConsListStack<A> empty() {
    return new ConsListStack<>(List.nil());
  }

  @Override
  public boolean equals(Object rhs) {
    return rhs instanceof ConsListStack<?> && _list.equals(((ConsListStack<?>)rhs)._list);
  }

  @Override
  public String toString() {
    return _list.toString();
  }

  @Override
  public Stack<A> push(A val) {
    return new ConsListStack<>(List.cons(val, _list));
  }

  @Override
  public Maybe<Pair<A, Stack<A>>> pop() {
    return _list.uncons().map(Pair.match((a, as) -> Pair.of(a, new ConsListStack<>(as))));
  }

  @Override
  public boolean isEmpty() {
    return _list.isEmpty();
  }
}
