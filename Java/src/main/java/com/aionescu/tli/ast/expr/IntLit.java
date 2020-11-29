package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.type.TInt;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.*;
import com.aionescu.tli.utils.data.map.Map;

import java.math.BigInteger;

import com.aionescu.tli.ast.Ident;

public final class IntLit implements Expr {
  private final BigInteger _val;

  public IntLit(BigInteger val) {
    _val = val;
  }

  @Override
  public String toString() {
    return String.valueOf(_val);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    return TInt.t;
  }

  @Override
  public Val eval(Map<Integer, Val> heap, Map<Ident, Val> sym) {
    return new VInt(_val);
  }
}
