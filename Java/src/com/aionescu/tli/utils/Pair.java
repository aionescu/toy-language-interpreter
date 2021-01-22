package com.aionescu.tli.utils;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

public final class Pair<A, B> {
  public final A fst;
  public final B snd;

  public Pair(A fst, B snd) {
    this.fst = fst;
    this.snd = snd;
  }

  public static <A, B> Pair<A, B> of(A fst, B snd) {
    return new Pair<>(fst, snd);
  }

  public static <A, B, C> Function<Pair<A, B>, C> match(BiFunction<A, B, C> f) {
    return p -> f.apply(p.fst, p.snd);
  }

  public static <A, B> Consumer<Pair<A, B>> matchDo(BiConsumer<A, B> f) {
    return p -> f.accept(p.fst, p.snd);
  }

  @Override
  public boolean equals(Object rhs) {
    if (!(rhs instanceof Pair<?, ?>))
      return false;

    var p = (Pair<?, ?>)rhs;
    return fst.equals(p.fst) && snd.equals(p.snd);
  }

  @Override
  public String toString() {
    return String.format("(%s, %s)", fst, snd);
  }

  public A fst_() {
    return fst;
  }

  public B snd_() {
    return snd;
  }

  public static <A, B, C> Function<Pair<A, B>, Pair<C, B>> first(Function<A, C> f) {
    return p -> Pair.of(f.apply(p.fst), p.snd);
  }

  public static <A, B, C> Function<Pair<A, B>, Pair<A, C>> second(Function<B, C> f) {
    return p -> Pair.of(p.fst, f.apply(p.snd));
  }
}
