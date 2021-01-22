package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ThreadState;
import com.aionescu.tli.ast.type.TStr;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VStr;
import com.aionescu.tli.exn.eval.FileNotOpenedException;
import com.aionescu.tli.utils.data.map.Map;

public final class Close implements Stmt {
  private final Expr _file;

  public Close(Expr file) {
    _file = file;
  }

  @Override
  public String toString() {
    return String.format("close %s", _file);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    _file.typeCheck(sym).mustBe(TStr.t);
    return sym;
  }

  @Override
  public ThreadState eval(ThreadState prog) {
    return prog.updateGlobal(g -> {
      var str = ((VStr)_file.eval(g.heap, prog.sym)).val;
      g.open.lookup(str).unwrap(() -> new FileNotOpenedException(str));

      return g.withOpen(g.open.delete(str));
    });
  }
}
