package com.aionescu.tli.utils.data.stack;

import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.data.list.List;

public interface Stack<A> {
  static <A> Stack<A> empty() {
    return ConsListStack.empty();
  }

  static <A> Stack<A> of(A val) {
    return Stack.<A>empty().push(val);
  }

  Stack<A> push(A val);
  Maybe<Pair<A, Stack<A>>> pop();

  boolean isEmpty();

  List<A> toList();
}
