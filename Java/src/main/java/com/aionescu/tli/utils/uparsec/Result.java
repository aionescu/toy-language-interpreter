package com.aionescu.tli.utils.uparsec;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import com.aionescu.tli.utils.Unit;

public abstract class Result<A> {
  private static final class Failure<A> extends Result<A> {
    @Override
    public <B> B match(Supplier<B> failure, BiFunction<A, String, B> success) {
      return failure.get();
    }
  }

  private static final class Success<A> extends Result<A> {
    private final A _result;
    private final String _rest;

    public Success(A result, String rest) {
      _result = result;
      _rest = rest;
    }

    @Override
    public <B> B match(Supplier<B> failure, BiFunction<A, String, B> success) {
      return success.apply(_result, _rest);
    }
  }

  public abstract <B> B match(Supplier<B> failure, BiFunction<A, String, B> success);

  public final void matchDo(Runnable failure, BiConsumer<A, String> success) {
    match(
      () -> { failure.run(); return Unit.UNIT; },
      (a, rest) -> { success.accept(a, rest); return Unit.UNIT; });
  }

  public static <A> Result<A> fail() {
    return new Failure<A>();
  }

  public static <A> Result<A> of(A result, String rest) {
    return new Success<A>(result, rest);
  }

  public final boolean isSuccess() {
    return match(() -> false, (a, s) -> true);
  }
}
