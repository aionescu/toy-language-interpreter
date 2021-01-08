package com.aionescu.tli.utils.control;

import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import com.aionescu.tli.utils.TriFunction;
import com.aionescu.tli.utils.Unit;

public abstract class Maybe<A> {
  private static final class NothingUnwrappedException extends RuntimeException {
    private static final long serialVersionUID = 1;

    @Override
    public String getMessage() {
      return "A Maybe.Nothing value was unwrapped.";
    }
  }

  private static final class Nothing<A> extends Maybe<A> {
    public Nothing() { }

    @Override
    public boolean equals(Object rhs) {
      return rhs instanceof Nothing<?>;
    }

    @Override
    public <B> B match(Supplier<B> nothing, Function<A, B> just) {
      return nothing.get();
    }
  }

  private static final class Just<A> extends Maybe<A> {
    private final A _val;

    public Just(A val) {
      _val = val;
    }

    @Override
    public boolean equals(Object rhs) {
      return rhs instanceof Just<?> && _val.equals(((Just<?>)rhs)._val);
    }

    @Override
    public <B> B match(Supplier<B> nothing, Function<A, B> just) {
      return just.apply(_val);
    }
  }

  private Maybe() { }

  @Override
  public abstract boolean equals(Object rhs);

  public abstract <B> B match(Supplier<B> nothing, Function<A, B> just);

  public final void matchDo(Runnable nothing, Consumer<A> just) {
    match(
      () -> { nothing.run(); return Unit.UNIT; },
      a -> { just.accept(a); return Unit.UNIT; });
  }

  public static <A> Maybe<A> nothing() {
    return new Nothing<>();
  }

  public static <A> Maybe<A> just(A val) {
    return new Just<>(val);
  }

  public static <A, B> Maybe<B> ap(Maybe<Function<A, B>> mf, Maybe<A> ma) {
    return mf.match(Maybe::nothing, f -> ma.match(Maybe::nothing, a -> Maybe.just(f.apply(a))));
  }

  public static <E, A, B, C> Maybe<C> liftA2(BiFunction<A, B, C> f, Maybe<A> ma, Maybe<B> mb) {
    return ma.match(Maybe::nothing, a -> mb.match(Maybe::nothing, b -> Maybe.just(f.apply(a, b))));
  }

  public static <E, A, B, C, D> Maybe<D> liftA3(TriFunction<A, B, C, D> f, Maybe<A> ma, Maybe<B> mb, Maybe<C> ec) {
    return ma.match(Maybe::nothing, a -> mb.match(Maybe::nothing, b -> ec.match(Maybe::nothing, c -> Maybe.just(f.apply(a, b, c)))));
  }

  public final <B> Maybe<B> map(Function<A, B> f) {
    return match(Maybe::nothing, a -> Maybe.just(f.apply(a)));
  }

  public final <B> Maybe<B> map_(B b) {
    return map(a -> b);
  }

  public final <B> Maybe<B> bind(Function<A, Maybe<B>> f) {
    return match(Maybe::nothing, f);
  }

  public final <B> Maybe<B> _then(Maybe<B> next) {
    return liftA2((a, b) -> b, this, next);
  }

  public final <B> Maybe<A> then_(Maybe<B> next) {
    return liftA2((a, b) -> a, this, next);
  }

  public final A unwrap(Supplier<? extends RuntimeException> e) {
    return match(() -> { throw e.get(); }, a -> a);
  }

  public final A unwrap() {
    return unwrap(NothingUnwrappedException::new);
  }
}
