package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ThreadState;
import com.aionescu.tli.ast.type.TStr;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.type.varinfo.VarState;
import com.aionescu.tli.ast.val.VStr;
import com.aionescu.tli.exn.eval.FileNotOpenedException;
import com.aionescu.tli.exn.eval.ReachedEOFException;
import com.aionescu.tli.exn.eval.ReadDifferentTypeException;
import com.aionescu.tli.exn.typeck.UndeclaredVariableException;
import com.aionescu.tli.utils.data.map.Map;

public final class Read implements Stmt {
  private final Ident _ident;
  private final Type _type;
  private final Expr _file;

  public Read(Ident ident, Type type, Expr file) {
    _ident = ident;
    _type = type;
    _file = file;
  }

  @Override
  public String toString() {
    return String.format("%s: %s = read %s", _ident, _type, _file);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    _file.typeCheck(sym).mustBe(TStr.t);

    var typ = sym.lookup(_ident).match(
      () -> { throw new UndeclaredVariableException(_ident); },
      vi -> vi.type);

    typ.mustBe(_type);
    typ.mustBeTransparent();

    return sym.insert(_ident, new VarInfo(typ, VarState.INIT));
  }

  @Override
  public ThreadState eval(ThreadState prog) {
    var str = ((VStr)_file.eval(prog.global.get().heap, prog.sym)).val;

    var global = prog.global.get();
    return global.open.lookup(str).match(
      () -> { throw new FileNotOpenedException(str); },
      l -> l.match(
        () -> { throw new ReachedEOFException(str); },
        (c, cs) -> {
          var tc = c.type();
          if (!tc.equals(_type))
            throw new ReadDifferentTypeException(str, tc, _type);

          prog.global.getAndUpdate(g -> g.withOpen(g.open.insert(str, cs)));
          return prog.withSym(prog.sym.insert(_ident, c));
        }));
  }
}
