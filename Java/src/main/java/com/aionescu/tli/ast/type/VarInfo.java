package com.aionescu.tli.ast.type;

public final class VarInfo {
  public final Type type;
  public final VarState state;

  public static VarInfo of(Type type, VarState state) {
    return new VarInfo(type, state);
  }

  public VarInfo(Type type, VarState state) {
    this.type = type;
    this.state = state;
  }
}
