package com.aionescu.tli.ast.stmt;

import com.aionescu.tli.utils.collections.map.Map;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.varinfo.VarInfo;

public interface Stmt {
  Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym);
  ProgState eval(ProgState prog);
}
