package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.ThreadState;
import com.aionescu.tli.ast.type.TInt;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.type.varinfo.VarState;
import com.aionescu.tli.ast.val.VInt;
import com.aionescu.tli.exn.typeck.UndeclaredVariableException;
import com.aionescu.tli.exn.typeck.UninitializedVariableException;
import com.aionescu.tli.utils.data.map.Map;

public final class Await implements Stmt {
  private final Ident _ident;

  public Await(Ident ident) {
    _ident = ident;
  }

  @Override
  public String toString() {
    return String.format("await %s", _ident);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    var varInfo = sym.lookup(_ident).unwrap(() -> new UndeclaredVariableException(_ident));

    if (varInfo.state == VarState.UNINIT)
      throw new UninitializedVariableException(_ident);

    varInfo.type.mustBe(TInt.t);
    return sym;
  }

  @Override
  public ThreadState eval(ThreadState prog) {
    var val = prog.sym.lookup(_ident).unwrap();
    var location = ((VInt)val).val.intValue();

    var oldG = prog.global.getAndUpdate(g ->
      g.withBarrierTable(g.barrierTable.addThread(location, prog.id)));

    var barrier = oldG.barrierTable.lookup(location).unwrap();
    var n1 = barrier.fst;
    var nl = barrier.snd.length();

    return n1 > nl ? prog.withToDo(prog.toDo.push(this)) : prog;
  }
}
