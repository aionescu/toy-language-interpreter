package com.aionescu.tli.repo;

import java.util.concurrent.atomic.AtomicReference;

import com.aionescu.tli.ast.prog.GlobalState;
import com.aionescu.tli.utils.control.Maybe;

public interface Repository {
  AtomicReference<GlobalState> state();
  void setState(AtomicReference<GlobalState> global);

  void oneStep();
  boolean done();

  void logState();
  void setLogPath(Maybe<String> path);
}
