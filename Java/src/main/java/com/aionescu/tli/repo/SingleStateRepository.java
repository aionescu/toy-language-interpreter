package com.aionescu.tli.repo;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.exn.eval.EvaluationFinishedException;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.map.Map;
import com.aionescu.tli.utils.control.Maybe;

public final class SingleStateRepository implements Repository {
  private ProgState _state;
  private Maybe<PrintWriter> _logFile = Maybe.nothing();

  @Override
  public ProgState state() {
    return _state;
  }

  @Override
  public void setState(ProgState state) {
    _state = state;
  }

  @Override
  public void typeCheck() {
    _state.toDo.foldl((sym, stmt) -> stmt.typeCheck(sym), Map.<Ident, VarInfo>empty());
  }

  @Override
  public void oneStep() {
    _state = _state.toDo.pop().match(
      () -> { throw new EvaluationFinishedException(); },
      Pair.match((stmt, toDo) -> stmt.eval(_state.withToDo(toDo))));
  }

  @Override
  public boolean done() {
    return _state.toDo.isEmpty();
  }

  @Override
  public void logState() {
    _logFile.matchDo(
      () -> { },
      f -> {
        f.write(_state.toString());
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
