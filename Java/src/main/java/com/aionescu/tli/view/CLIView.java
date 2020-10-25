package com.aionescu.tli.view;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.aionescu.tli.exn.eval.EvalException;
import com.aionescu.tli.exn.typeck.TypeCheckerException;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.controller.*;
import com.aionescu.tli.parser.TLParser;
import com.aionescu.tli.utils.collections.stack.Stack;
import com.aionescu.tli.utils.uparsec.exn.UParsecException;

public final class CLIView implements View {
  private final Controller _controller;

  public CLIView(Controller controller) {
    _controller = controller;
  }

  private String _getCode(String path) throws IOException {
    return switch (path) {
      case "-" -> new String(System.in.readAllBytes());
      default -> Files.readString(Path.of(path));
    };
  }

  private void _runSmallStep(String code) {
    var ast = TLParser.parse(code);
    var prog = ProgState.empty.withToDo(Stack.of(ast));

    _controller.setState(prog);
    _controller.typeCheck();

    System.out.println(_controller.state());

    while (!_controller.done())
      System.out.println(_controller.oneStep());
  }

  private void _runBigStep(String code) {
    var ast = TLParser.parse(code);
    var prog = ProgState.empty.withToDo(Stack.of(ast));

    _controller.setState(prog);
    _controller.typeCheck();

    _controller.allSteps();
    System.out.println(_controller.state().output());
  }

  private void _handleRun(String[] args) throws IOException {
    switch (args[1]) {
      case "--small-step" -> _runSmallStep(_getCode(args[2]));
      default -> _runBigStep(_getCode(args[1]));
    };
  }

  private void _dumpAST(String code, boolean typeCheck) {
    var ast = TLParser.parse(code);
    var prog = ProgState.empty.withToDo(Stack.of(ast));

    _controller.setState(prog);

    if (typeCheck)
      _controller.typeCheck();

    System.out.println(ast);
  }

  private void _handleDumpAST(String[] args) throws IOException {
    switch (args[1]) {
      case "--no-type-check" -> _dumpAST(_getCode(args[2]), false);
      default -> _dumpAST(_getCode(args[1]), true);
    }
  }

  private void _handleCommand(String[] args) throws IOException {
    switch (args[0]) {
      case "run" -> _handleRun(args);
      case "dump-ast" -> _handleDumpAST(args);
      default -> System.out.println(String.format("Unrecognized command %s.", args[0]));
    }
  }

  @Override
  public void run(String[] args) {
    try {
      _handleCommand(args);
    } catch (IOException e) {
      System.out.println("IO Error: " + e.getMessage());
    } catch (UParsecException e) {
      System.out.println("Parser error: " + e.getMessage());
    } catch (TypeCheckerException e) {
      System.out.println("Type error: " + e.getMessage());
    } catch (EvalException e) {
      System.out.println("Evaluation error: " + e.getMessage());
    } catch (Throwable e) {
      System.out.println("Something unexpected occurred:");
      e.printStackTrace();
    }
  }
}
