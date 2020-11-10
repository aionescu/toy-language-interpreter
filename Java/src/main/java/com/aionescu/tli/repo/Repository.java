package com.aionescu.tli.repo;

import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.utils.control.Maybe;

public interface Repository {
  ProgState state();
  void setState(ProgState state);

  void typeCheck();
  void oneStep();
  boolean done();

  void logState();
  void setLogPath(Maybe<String> path);
}
