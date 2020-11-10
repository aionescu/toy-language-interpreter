package com.aionescu.tli.view;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Scanner;
import java.util.function.BiConsumer;

import com.aionescu.tli.exn.eval.EvalException;
import com.aionescu.tli.exn.typeck.TypeCheckerException;
import com.aionescu.tli.ast.prog.ProgState;
import com.aionescu.tli.controller.*;
import com.aionescu.tli.parser.TLParser;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.collections.list.List;
import com.aionescu.tli.utils.collections.map.Map;
import com.aionescu.tli.utils.collections.stack.Stack;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.uparsec.exn.UParsecException;

public final class TUIView implements View {
  private final Controller _controller;
  private static final Map<String, Pair<String, BiConsumer<TUIView, String>>> _commands = _getCommands();

  public TUIView(Controller controller) {
    _controller = controller;
  }

  private static BiConsumer<TUIView, String> _toLambda(Method m) {
    return (view, arg) -> {
      try {
        m.invoke(view, arg);
      } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
        e.printStackTrace();
      }
    };
  }

  private static Map<String, Pair<String, BiConsumer<TUIView, String>>> _getCommands() {
    var ann = Command.class;

    return
      Map.fromList(
        List.ofStream(
          Arrays.<Method>stream(
            TUIView.class.getDeclaredMethods())
          .filter(m -> m.isAnnotationPresent(ann))
          .map(m -> {
            var c = m.getAnnotation(ann);
            return Pair.of(c.name(), Pair.of(c.desc(), _toLambda(m)));
          })));
  }

  @Command(name = "exit", desc = "Exits the interactive interpreter.")
  private void _exit(String arg) {
    System.exit(0);
  }

  @Command(name = "help", desc = "Displays a list of available commands.")
  private void _showHelp(String arg) {
    System.out.println("Available commands:");
    _commands.toList().iter(kvp -> System.out.println(kvp.fst + ": " + kvp.snd.fst));

    System.out.println();
  }

  @Command(name = "load", desc = "Parses, typechecks and loads the specified TL file.")
  private void _loadCode(String arg) {
    try {
      var code = Files.readString(Path.of(arg));
      var ast = TLParser.parse(code);
      var prog = ProgState.empty.withToDo(Stack.of(ast));

      _controller.setState(prog);
      _controller.typeCheck();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  @Command(name = "run-all-steps", desc = "Runs the loaded program, displaying all intermediate states.")
  private void _runSmallStep(String arg) {
    System.out.println(_controller.state());

    while (!_controller.done())
      System.out.println(_controller.oneStep());
  }

  @Command(name = "run", desc = "Runs the loaded program, displaying its final output.")
  private void _runBigStep(String arg) {
    var state = _controller.state();

    try {
      _controller.allSteps();
      System.out.println(_controller.state().output());
    } finally {
      _controller.setState(state);
    }
  }

  @Command(name = "show-ast", desc = "Shows the AST of the loaded program.")
  private void _dumpAST(String arg) {
    System.out.println(_controller.state().toDo);
  }

  @Command(name = "parse", desc = "Reads a program from its argument, then parses, typechecks, and loads it.")
  private void _parseStdin(String arg) {
    var ast = TLParser.parse(arg);
    var prog = ProgState.empty.withToDo(Stack.of(ast));

    _controller.setState(prog);
    _controller.typeCheck();
  }

  @Command(name = "set-log-file", desc = "Sets the log file to the specified path.")
  private void _SetLogPath(String arg) {
    _controller.setLogPath(Maybe.just(arg));
  }

  @Command(name = "unset-log-file", desc = "Unsets the log file, causing the repository to stop logging.")
  private void _unsetLogPath(String arg) {
    _controller.setLogPath(Maybe.nothing());
  }

  private void _dispatch(String cmd, String arg) {
    _commands.lookup(cmd).matchDo(
      () -> System.out.println("Command \"" + cmd + "\" not recognized."),
      c -> c.snd.accept(this, arg));
  }

  @Override
  public void run() {
    try (var in = new Scanner(System.in)) {
      while (true) {
        try {
          System.out.print("tli> ");

          var line = in.nextLine();
          var parts = line.split(" ", 2);

          if (parts.length > 0)
            _dispatch(parts[0], parts.length > 1 ? parts[1] : "");

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
  }
}
