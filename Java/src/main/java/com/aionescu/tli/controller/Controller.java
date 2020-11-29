package com.aionescu.tli.controller;

import com.aionescu.tli.ast.prog.GlobalState;
import com.aionescu.tli.repo.Repository;
import com.aionescu.tli.utils.control.Maybe;
import java.util.concurrent.atomic.AtomicReference;

public final class Controller {
  Repository _repo;

  public Controller(Repository repo) {
    _repo = repo;
  }

  public AtomicReference<GlobalState> state() {
    return _repo.state();
  }

  public void setState(AtomicReference<GlobalState> state) {
    _repo.setState(state);
  }

  public void oneStep() {
    _repo.oneStep();
  }

  public boolean done() {
    return _repo.done();
  }

  public void setLogPath(Maybe<String> path) {
    _repo.setLogPath(path);
  }
}
