package com.aionescu.tli.ast.prog;

import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.collections.map.Map;
import com.aionescu.tli.utils.collections.stack.Stack;
import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.stmt.Stmt;
import com.aionescu.tli.ast.val.Val;

public final class ProgState {
  public static final ProgState empty = new ProgState(Map.empty(), Stack.empty(), Map.empty(), List.nil());

  public final Map<String, List<Val>> open;
  public final Stack<Stmt> toDo;
  public final Map<Ident, Val> sym;
  public final List<Val> out;

  private ProgState(Map<String, List<Val>> open, Stack<Stmt> toDo, Map<Ident, Val> sym, List<Val> out) {
    this.open = open;
    this.toDo = toDo;
    this.sym = sym;
    this.out = out;
  }

  public ProgState withOpen(Map<String, List<Val>> open) {
    return new ProgState(open, this.toDo, this.sym, this.out);
  }

  public ProgState withToDo(Stack<Stmt> toDo) {
    return new ProgState(this.open, toDo, this.sym, this.out);
  }

  public ProgState withSym(Map<Ident, Val> sym) {
    return new ProgState(this.open, this.toDo, sym, this.out);
  }

  public ProgState withOut(List<Val> out) {
    return new ProgState(this.open, this.toDo, this.sym, out);
  }

  public String output() {
    return out.reverse().unlines();
  }

  @Override
  public String toString() {
    return String.format("toDo = %s\nsym = %s\nout = %s\n", toDo, sym, out.reverse());
  }
}
