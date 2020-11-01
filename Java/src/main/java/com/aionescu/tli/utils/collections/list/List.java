package com.aionescu.tli.utils.collections.list;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Stream;

import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.stack.Stack;
import com.aionescu.tli.utils.control.Maybe;

public abstract class List<A> implements Stack<A> {
  private static final class Nil<A> extends List<A> {
    public Nil() { }

    @Override
    public <B> B match(Supplier<B> nil, BiFunction<A, List<A>, B> cons) {
      return nil.get();
    }

    @Override
    public void matchDo(Runnable nil, BiConsumer<A, List<A>> cons) {
      nil.run();
    }
  }

  private static final class Cons<A> extends List<A> {
    private final A _head;
    private final List<A> _tail;

    public Cons(A head, List<A> tail) {
      _head = head;
      _tail = tail;
    }

    @Override
    public <B> B match(Supplier<B> nil, BiFunction<A, List<A>, B> cons) {
      return cons.apply(_head, _tail);
    }

    @Override
    public void matchDo(Runnable nil, BiConsumer<A, List<A>> cons) {
      cons.accept(_head, _tail);
    }
  }

  private List() { }

  public abstract <B> B match(Supplier<B> nil, BiFunction<A, List<A>, B> cons);
  public abstract void matchDo(Runnable nil, BiConsumer<A, List<A>> cons);

  @Override
  public final String toString() {
    return toString("[", "]");
  }

  @Override
  public final List<A> push(A val) {
    return List.cons(val, this);
  }

  @Override
  public final Maybe<Pair<A, Stack<A>>> pop() {
    return match(Maybe::nothing, (h, t) -> Maybe.just(Pair.of(h, t)));
  }

  @Override
  public final boolean isEmpty() {
    return match(() -> true, (h, t) -> false);
  }

  @Override
  public final <S> S foldl(BiFunction<S, A, S> f, S s) {
    return match(() -> s, (h, t) -> t.foldl(f, f.apply(s, h)));
  }

  public static <A> List<A> nil() {
    return new Nil<>();
  }

  public static <A> List<A> cons(A head, List<A> tail) {
    return new Cons<>(head, tail);
  }

  public static <A> List<A> singleton(A value) {
    return cons(value, nil());
  }

  public static <A> List<A> ofStream(Stream<A> stream) {
    return stream.reduce(List.<A>nil(), (l, a) -> cons(a, l), List::append).reverse();
  }

  private static String _asString(List<Character> chars, String acc) {
    return chars.match(() -> acc, (h, t) -> _asString(t, acc + h));
  }

  public static String asString(List<Character> chars) {
    return _asString(chars, "");
  }

  public final String toString(String begin, String end) {
    return match(
      () -> begin + end,
      (h, t) -> begin + h + t.foldl((s, a) -> s + ", " + a, "") + end);
  }

  public final List<A> append(List<A> b) {
    return match(() -> b, (h, t) -> List.cons(h, t.append(b)));
  }

  public final <B> List<B> map(Function<A, B> f) {
    return match(() -> nil(), (h, t) -> List.cons(f.apply(h), t.map(f)));
  }

  public final void iter(Consumer<A> f) {
    matchDo(() -> { }, (h, t) -> { f.accept(h); t.iter(f); });
  }

  public final <S> S foldr(BiFunction<A, S, S> f, S s) {
    return match(() -> s, (h, t) -> f.apply(h, t.foldr(f, s)));
  }

  public final List<A> reverse() {
    return match(() -> this, (h, t) -> t.reverse().append(List.cons(h, List.nil())));
  }

  public final String unlines() {
    return match(
      () -> "",
      (h, t) -> h.toString() + t.foldl((s, a) -> s + "\n" + a, ""));
  }
}
