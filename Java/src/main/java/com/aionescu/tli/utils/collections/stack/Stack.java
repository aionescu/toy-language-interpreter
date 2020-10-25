package com.aionescu.tli.utils.collections.stack;

import java.util.function.BiFunction;

import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.control.Maybe;

public interface Stack<A> {
  static <A> Stack<A> empty() {
    return List.nil();
  }

  Stack<A> push(A val);
  Maybe<Pair<A, Stack<A>>> pop();

  boolean isEmpty();
  <S> S foldl(BiFunction<S, A, S> f, S s);
}
