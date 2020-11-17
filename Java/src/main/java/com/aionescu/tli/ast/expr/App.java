package com.aionescu.tli.ast.expr;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.TFun;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VFun;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.exn.typeck.ExpectedFunFoundException;
import com.aionescu.tli.utils.collections.map.Map;

public final class App implements Expr {
  private final Expr _fn, _arg;

  public App(Expr fn, Expr arg) {
    _fn = fn;
    _arg = arg;
  }

  @Override
  public String toString() {
    return String.format("(%s %s)", _fn, _arg);
  }

  @Override
  public Type typeCheck(Map<Ident, VarInfo> sym) {
    var fn = _fn.typeCheck(sym);
    var arg = _arg.typeCheck(sym);

    if (!(fn instanceof TFun))
      throw new ExpectedFunFoundException(fn);

    var tfun = (TFun)fn;
    arg.mustBe(tfun.in);

    return tfun.out;
  }

  @Override
  public Val eval(Map<Integer, Val> heap, Map<Ident, Val> sym) {
    var fn = (VFun)_fn.eval(heap, sym);
    var arg = _arg.eval(heap, sym);

    return fn.f.apply(fn.sym, arg);
  }
}
