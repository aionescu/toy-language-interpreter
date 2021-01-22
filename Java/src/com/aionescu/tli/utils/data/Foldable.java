package com.aionescu.tli.utils.data;

import java.util.function.BiFunction;

public interface Foldable<A> {
  <S> S foldL(BiFunction<S, A, S> f, S zero);
  <S> S foldR(BiFunction<A, S, S> f, S zero);
}
