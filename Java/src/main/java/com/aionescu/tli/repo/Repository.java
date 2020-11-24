package com.aionescu.tli.repo;

import com.aionescu.tli.ast.prog.GlobalState;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.control.Ref;

public interface Repository {
  Ref<GlobalState> state();
  void setState(Ref<GlobalState> global);

  void oneStep();
  boolean done();

  void logState();
  void setLogPath(Maybe<String> path);
}
