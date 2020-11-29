package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.ThreadState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.utils.data.list.List;
import com.aionescu.tli.utils.data.map.Map;
import com.aionescu.tli.utils.data.stack.Stack;

public final class Fork implements Stmt {
  private final Stmt _stmt;

  public Fork(Stmt stmt) {
    _stmt = stmt;
  }

  @Override
  public String toString() {
    return String.format("fork { %s }", _stmt);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    _stmt.typeCheck(sym);
    return sym;
  }

  @Override
  public ThreadState eval(ThreadState prog) {
    synchronized (prog.global) {
      var global = prog.global.get();

      var newThread = ThreadState.initial(prog.global, global.nextID).withToDo(Stack.of(_stmt)).withSym(prog.sym);
      prog.global.update(g ->
        g
        .withThreads(List.cons(newThread, g.threads))
        .withNextID(g.nextID + 1));

      return prog;
    }
  }
}
