package com.aionescu.tli.repo;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.exn.eval.EvaluationFinishedException;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.map.Map;

public final class SingleStateRepository implements Repository {
  private ProgState _state;

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
}
