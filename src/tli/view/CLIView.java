package tli.view;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import tli.exn.ParserException;
import tli.exn.eval.EvalException;
import tli.exn.typeck.TypeCheckerException;
import tli.ast.prog.ProgState;
import tli.controller.*;
import tli.parser.TLIParser;
import utils.collections.list.List;

public final class CLIView implements View {
  private final Controller _controller;

  public CLIView(Controller controller) {
    _controller = controller;
  }

  private void _handleCommand(String line) throws IOException {
    var parts = line.split("\\s+", 2);

    switch (parts[0]) {
      case "load":
        var code = Files.readString(Path.of(parts[1]));
        var ast = TLIParser.parse(code);
        _controller.setState(ProgState.empty.withToDo(List.singleton(ast)));
        break;

      case "typeck":
        _controller.typeCheck();
        break;

      case "show":
        System.out.println(_controller.state());
        break;

      case "run":
        var state = _controller.state();

        _controller.allSteps();
        System.out.println(_controller.state().output());

        _controller.setState(state);
        break;

      case "all-steps":
        _controller.allSteps().map(Object::toString).iter(System.out::println);
        break;

      case "one-step":
        _controller.oneStep();
        System.out.println(_controller.state());
        break;

      default:
        System.out.println("Unrecognized command");
        break;
    }
  }

  @Override
  public void run() {
    var console = System.console();

    while (true) {
      System.out.print("\ntli> ");
      var line = console.readLine();

      try {
        _handleCommand(line);
      } catch (IOException e) {
        System.out.println("IO Error: " + e.getMessage());
      } catch (ParserException e) {
        System.out.println("Parser error: " + e.getMessage());
      } catch (TypeCheckerException e) {
        System.out.println("Type error: " + e.getMessage());
      } catch (EvalException e) {
        System.out.println("Evaluation error: " + e.getMessage());
      }
    }
  }
}
