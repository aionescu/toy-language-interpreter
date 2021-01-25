package com.aionescu.tli.ast.stmt;

import java.math.BigInteger;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ThreadState;
import com.aionescu.tli.ast.type.TInt;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.type.varinfo.VarState;
import com.aionescu.tli.ast.val.VInt;
import com.aionescu.tli.utils.data.map.Map;

public final class NewBarrier implements Stmt {
  private final Ident _ident;
  private final Expr _expr;

  public NewBarrier(Ident ident, Expr expr) {
    _ident = ident;
    _expr = expr;
  }

  @Override
  public String toString() {
    return String.format("%s = barrier %s", _ident, _expr);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    Type.lookupVariable(sym, _ident).mustBe(TInt.t);

    _expr.typeCheck(sym).mustBe(TInt.t);
    return sym.insert(_ident, new VarInfo(TInt.t, VarState.INIT));
  }

  @Override
  public ThreadState eval(ThreadState prog) {
    var oldG = prog.global.getAndUpdate(g -> {
      var val = _expr.eval(g.heap, prog.sym);
      var count = ((VInt)val).val.intValue();

      return g.withBarrierTable(g.barrierTable.addBarrier(count));
    });

    return prog.withSym(prog.sym.insert(_ident, new VInt(BigInteger.valueOf(oldG.barrierTable.nextFreeLocation()))));
  }
}
