package com.aionescu.tli.utils.control;

import java.util.function.UnaryOperator;

public final class Ref<A> {
  private A _val;

  public Ref(A val) {
    _val = val;
  }

  public A get() {
    return _val;
  }

  public void set(A val) {
    _val = val;
  }

  public void update(UnaryOperator<A> f) {
    _val = f.apply(_val);
  }
}
