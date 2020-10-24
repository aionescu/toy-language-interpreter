package tli.ast.expr;

import utils.collections.map.Map;

import tli.ast.Ident;
import tli.ast.type.Type;
import tli.ast.type.VarInfo;
import tli.ast.val.Val;

public interface Expr {
  Type typeCheck(Map<Ident, VarInfo> sym);
  Val eval(Map<Ident, Val> sym);
}
