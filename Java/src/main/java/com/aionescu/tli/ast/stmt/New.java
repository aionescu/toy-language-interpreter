package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.GCStats;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.TRef;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.type.varinfo.VarState;
import com.aionescu.tli.ast.val.VRef;
import com.aionescu.tli.exn.eval.OutOfMemoryException;
import com.aionescu.tli.exn.typeck.UndeclaredVariableException;
import com.aionescu.tli.utils.collections.map.Map;

public final class New implements Stmt {
  private final Ident _ident;
  private final Expr _expr;

  public New(Ident ident, Expr expr) {
    _ident = ident;
    _expr = expr;
  }

  @Override
  public String toString() {
    return String.format("%s = new %s", _ident, _expr);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    var t = _expr.typeCheck(sym);
    var typ = sym.lookup(_ident).match(
      () -> { throw new UndeclaredVariableException(_ident); },
      a -> a.type);

    typ.mustBe(new TRef(t));
    return sym.insert(_ident, new VarInfo(typ, VarState.INIT));
  }

  @Override
  public ProgState eval(ProgState prog) {
    if (prog.gcStats.crrHeapSize == prog.gcStats.maxHeapSize)
      throw new OutOfMemoryException(prog.gcStats.maxHeapSize);

    var v = _expr.eval(prog.heap, prog.sym);
    var sym = prog.sym.insert(_ident, new VRef(prog.gcStats.crrHeapSize));
    var heap = prog.heap.insert(prog.gcStats.crrHeapSize, v);

    return GCStats.runGC(prog.withSym(sym).withHeap(heap));
  }
}
