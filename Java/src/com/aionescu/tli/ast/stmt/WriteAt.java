package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ThreadState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VRef;
import com.aionescu.tli.utils.data.map.Map;

public final class WriteAt implements Stmt {
  private final Expr _lhs, _rhs;

  public WriteAt(Expr lhs, Expr rhs) {
    _lhs = lhs;
    _rhs = rhs;
  }

  @Override
  public String toString() {
    return String.format("%s := %s", _lhs, _rhs);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    var tl = _lhs.typeCheck(sym).unwrapTRef();
    var tr = _rhs.typeCheck(sym);
    tr.mustBe(tl);

    return sym;
  }

  @Override
  public ThreadState eval(ThreadState prog) {
    return prog.updateGlobal(g -> {
      var vl = ((VRef)_lhs.eval(g.heap, prog.sym)).addr;
      var vr = _rhs.eval(g.heap, prog.sym);

      return g.withHeap(g.heap.insert(vl, vr));
    });
  }
}
