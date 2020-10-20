package tli;

import java.util.Arrays;

import tli.ast.Ident;
import tli.ast.expr.Arith;
import tli.ast.expr.Compare;
import tli.ast.expr.Lit;
import tli.ast.expr.Var;
import tli.ast.expr.Arith.Op;
import tli.ast.prog.ProgState;
import tli.ast.stmt.Assign;
import tli.ast.stmt.Comp;
import tli.ast.stmt.Decl;
import tli.ast.stmt.Print;
import tli.ast.stmt.Stmt;
import tli.ast.stmt.While;
import tli.ast.type.Type;
import tli.ast.val.Int;
import tli.controller.Controller;
import tli.repo.SingleStateRepository;
import tli.view.CLIView;
import utils.collections.list.List;

public final class Main {
  public static void main(String[] args) {
    Stmt[] stmts = {
      Decl.of(Type.INT, Ident.of("a")),
      Assign.of(Ident.of("a"), Lit.of(Int.of(0))),
      While.of(
        Compare.of(Var.of(Ident.of("a")), Compare.Op.LT, Lit.of(Int.of(10))),
        Comp.of(
          Print.of(Var.of(Ident.of("a"))),
          Assign.of(
            Ident.of("a"),
            Arith.of(Var.of(Ident.of("a")), Arith.Op.ADD, Lit.of(Int.of(1))))))
    };

    var state = ProgState.empty.withToDo(List.ofStream(Arrays.stream(stmts)));

    var repo = new SingleStateRepository(state);
    var controller = new Controller(repo);
    var view = new CLIView(controller);

    view.run();
  }
}
