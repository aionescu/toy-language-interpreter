package com.aionescu.tli.ast.expr;

import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.kind.ExprKind;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.Val;

public interface Expr<K extends ExprKind> {
  Type typeCheck(Map<Ident, VarInfo> sym);
  Val eval(Map<Ident, Val> sym);
}
