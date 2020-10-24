package com.aionescu.tli.ast.prog;

import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.stmt.Stmt;
import com.aionescu.tli.ast.val.Val;

public final class ProgState {
  public final List<Stmt> toDo;
  public final Map<Ident, Val> sym;
  public final List<String> out;

  public static final ProgState empty = new ProgState(List.nil(), Map.empty(), List.nil());

  private ProgState(List<Stmt> toDo, Map<Ident, Val> sym, List<String> out) {
    this.toDo = toDo;
    this.sym = sym;
    this.out = out;
  }

  public ProgState withToDo(List<Stmt> toDo) {
    return new ProgState(toDo, this.sym, this.out);
  }

  public ProgState withSym(Map<Ident, Val> sym) {
    return new ProgState(this.toDo, sym, this.out);
  }

  public ProgState withOut(List<String> out) {
    return new ProgState(this.toDo, this.sym, out);
  }

  public String output() {
    return out.reverse().unlines();
  }

  @Override
  public String toString() {
    return String.format("toDo = %s\nsym = %s\nout = %s\n", toDo, sym, out);
  }
}
