package com.aionescu.tli.ast.prog;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.stmt.Stmt;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.eval.EvaluationFinishedException;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.data.list.List;
import com.aionescu.tli.utils.data.map.Map;
import com.aionescu.tli.utils.data.stack.Stack;
import java.util.concurrent.atomic.AtomicReference;

public final class ThreadState {
  public final AtomicReference<GlobalState> global;
  public final int id;
  public final Stack<Stmt> toDo;
  public final Map<Ident, Val> sym;

  private ThreadState(AtomicReference<GlobalState> global, int id, Stack<Stmt> toDo, Map<Ident, Val> sym) {
    this.global = global;
    this.id = id;
    this.toDo = toDo;
    this.sym = sym;
  }

  public ThreadState withToDo(Stack<Stmt> toDo) {
    return new ThreadState(global, id, toDo, sym);
  }

  public ThreadState withSym(Map<Ident, Val> sym) {
    return new ThreadState(global, id, toDo, sym);
  }

  public ThreadState eval() {
    return toDo.pop().match(
      () -> { throw new EvaluationFinishedException(); },
      Pair.match((stmt, toDo) -> stmt.eval(this.withToDo(toDo))));
  }

  public boolean isNotDone() {
    return !toDo.isEmpty();
  }

  public static ThreadState initial(AtomicReference<GlobalState> global, int id) {
    return new ThreadState(global, id, Stack.empty(), Map.empty());
  }

  public static String showThreads(List<ThreadState> threads) {
    return threads.map(Object::toString).unlines();
  }

  @Override
  public String toString() {
    return String.format("  id = %s\n  toDo = %s\n  sym = %s\n", id, toDo, sym);
  }
}
