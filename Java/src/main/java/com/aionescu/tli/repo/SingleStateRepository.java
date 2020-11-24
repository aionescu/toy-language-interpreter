package com.aionescu.tli.repo;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import com.aionescu.tli.ast.prog.GlobalState;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.control.Ref;

public final class SingleStateRepository implements Repository {
  private Ref<GlobalState> _global = new Ref<>(GlobalState.empty);
  private Maybe<PrintWriter> _logFile = Maybe.nothing();

  @Override
  public Ref<GlobalState> state() {
    return _global;
  }

  @Override
  public void setState(Ref<GlobalState> state) {
    _global = state;
  }

  @Override
  public void oneStep() {
    logState();

    GlobalState.eval(_global);

    if (done())
      logState();
  }

  @Override
  public boolean done() {
    return _global.get().threads.isEmpty();
  }

  @Override
  public void logState() {
    _logFile.matchDo(
      () -> { },
      f -> {
        f.write(_global.get().toString());
        f.write("\n");
        f.flush();
      }
    );
  }

  @Override
  public void setLogPath(Maybe<String> path) {
    path.matchDo(
      () -> _logFile = Maybe.nothing(),
      p -> {
        try {
          _logFile = Maybe.just(new PrintWriter(new BufferedWriter(new FileWriter(p, true))));
        } catch (IOException e) {
          e.printStackTrace();
        }
      }
    );
  }
}
