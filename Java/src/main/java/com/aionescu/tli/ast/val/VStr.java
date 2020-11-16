package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.type.TStr;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.exn.eval.PanicException;

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
    if (!(rhs instanceof VStr))
      throw new PanicException();

    return val.compareTo(((VStr)rhs).val);
  }

  @Override
  public Type type() {
    return TStr.t;
  }
}
