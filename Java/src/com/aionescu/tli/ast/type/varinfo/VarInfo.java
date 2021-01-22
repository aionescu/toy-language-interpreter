package com.aionescu.tli.ast.type.varinfo;

import com.aionescu.tli.ast.type.Type;

public final class VarInfo {
  public final Type type;
  public final VarState state;

  public VarInfo(Type type, VarState state) {
    this.type = type;
    this.state = state;
  }
}
