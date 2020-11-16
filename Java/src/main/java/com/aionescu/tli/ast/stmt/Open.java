package com.aionescu.tli.ast.stmt;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.ast.type.TStr;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VStr;
import com.aionescu.tli.exn.eval.FileAlreadyOpenedException;
import com.aionescu.tli.parser.TLParser;
import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.collections.map.Map;

public final class Open implements Stmt {
  private final Expr _file;

  public Open(Expr file) {
    _file = file;
  }

  @Override
  public String toString() {
    return String.format("open %s", _file);
  }

  @Override
  public Map<Ident, VarInfo> typeCheck(Map<Ident, VarInfo> sym) {
    _file.typeCheck(sym).mustBe(TStr.t);
    return sym;
  }

  @Override
  public ProgState eval(ProgState prog) {
    var str = ((VStr)_file.eval(prog.sym)).val;

    prog.open.lookup(str).matchDo(
      () -> { },
      a -> { throw new FileAlreadyOpenedException(str); });

    String contents;

    try {
      contents = Files.readString(Path.of(str));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    var vals =
      List
      .ofStream(
        Arrays.stream(
          contents.split("\n")))
      .map(l -> TLParser.parseValLine(str, l));

    return prog.withOpen(prog.open.insert(str, vals));
  }
}
