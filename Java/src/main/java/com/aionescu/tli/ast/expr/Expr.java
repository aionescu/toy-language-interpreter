package com.aionescu.tli.ast.expr;

import com.aionescu.tli.utils.data.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.type.Type;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.Val;

public interface Expr {
  Type typeCheck(Map<Ident, VarInfo> sym);
  Val eval(Map<Integer, Val> heap, Map<Ident, Val> sym);
}
