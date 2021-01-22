package com.aionescu.tli.ast.stmt;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Arrays;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.expr.Expr;
import com.aionescu.tli.ast.prog.ThreadState;
import com.aionescu.tli.ast.type.TStr;
import com.aionescu.tli.ast.type.varinfo.VarInfo;
import com.aionescu.tli.ast.val.VStr;
import com.aionescu.tli.exn.eval.FileAlreadyOpenedException;
import com.aionescu.tli.exn.eval.FileDoesNotExistException;
import com.aionescu.tli.parser.TLParser;
import com.aionescu.tli.utils.data.list.List;
import com.aionescu.tli.utils.data.map.Map;

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
  public ThreadState eval(ThreadState prog) {
    var str = ((VStr)_file.eval(prog.global.get().heap, prog.sym)).val;

    var global = prog.global.get();

    global.open.lookup(str).matchDo(
      () -> { },
      a -> { throw new FileAlreadyOpenedException(str); });

    String contents;

    try {
      contents = Files.readString(Path.of(str));
    } catch (NoSuchFileException e) {
      throw new FileDoesNotExistException(str);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    var vals =
      List
      .ofStream(
        Arrays.stream(
          contents.split("\n")))
      .map(l -> TLParser.parseValLine(str, l));

    prog.global.getAndUpdate(g -> g.withOpen(g.open.insert(str, vals)));
    return prog;
  }
}
