package com.aionescu.tli.repo;

import com.aionescu.tli.ast.prog.ProgState;

public interface Repository {
  ProgState state();
  void setState(ProgState state);
  void typeCheck();
  void oneStep();
  boolean done();
}
