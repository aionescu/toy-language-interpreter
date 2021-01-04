package com.aionescu.tli.ast.val;

import java.util.function.UnaryOperator;

import com.aionescu.tli.ast.type.TStr;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.utils.data.set.Set;

public final class VStr extends Val {
  public final String val;

  public VStr(String val) {
    this.val = val;
  }

  private static String _escape(String s) {
    return
      s
      .replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\0", "\\0")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
      .replace("\b", "\\b")
      .replace("\f", "\\f");
  }

  public static String escapeString(String s) {
    return "\"" + _escape(s) + "\"";
  }

  @Override
  public String toString() {
    return escapeString(val);
  }

  @Override
  public int compareTo(Val rhs) {
    return rhs instanceof VStr ? val.compareTo(((VStr)rhs).val) : -1;
  }

  @Override
  public Type type() {
    return TStr.t;
  }

  @Override
  public Set<Integer> getInnerAddrs() {
    return Set.empty();
  }

  @Override
  public Val mapInnerAddrs(UnaryOperator<Integer> f) {
    return this;
  }
}
