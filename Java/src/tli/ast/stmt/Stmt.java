package tli.ast.stmt;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.prog.ProgState;
import tli.ast.type.VarInfo;

public interface Stmt {
  Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym);
  ProgState eval(ProgState prog);
}
