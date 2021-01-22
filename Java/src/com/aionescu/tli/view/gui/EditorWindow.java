package com.aionescu.tli.view.gui;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import javafx.stage.FileChooser.ExtensionFilter;

import com.aionescu.tli.ast.stmt.Stmt;
import com.aionescu.tli.exn.typeck.TypeCheckerException;
import com.aionescu.tli.parser.TLParser;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.data.map.Map;
import com.aionescu.tli.utils.uparsec.exn.UParsecException;

import static com.aionescu.tli.view.gui.GUIWindow.*;

public final class EditorWindow implements GUIWindow {
  private final Stage _stage;
  private final Button _open, _save, _run;
  private final TextArea _editor;
  private final VBox _vbox;

  private Maybe<Path> _file = Maybe.nothing();

  public EditorWindow(Stage stage) {
    _stage = stage;
    _stage.setTitle("TL Playground");

    _open = mkButton("Open", () -> _open());

    _save = mkButton("Save", () -> _save());
    _save.setDisable(true);

    _run = mkButton("Run", () -> _run());
    _run.setDisable(true);

    _editor = new TextArea();
    _editor.setFont(new Font("Fira Code Regular", 18));
    _editor.setPrefSize(800, 750);

    _editor.setText("Please open a file.");
    _editor.setDisable(true);

    _editor.textProperty().addListener(e -> {
      _file.matchDo(
        () -> { },
        path -> _stage.setTitle("TL Playground - " + path.getFileName().toString() + " (*)"));
    });

    var hbox = new HBox(_open, _save, _run);
    _vbox = new VBox(hbox, _editor);

    _vbox.setOnKeyPressed(e -> {
      if (e.isControlDown())
        switch (e.getCode()) {
          case O -> _open();
          case S -> _save();
          case R -> _run();
          default -> { }
        }
    });
  }

  void _open() {
    var fileChooser = new FileChooser();

    fileChooser.setTitle("Choose File");
    fileChooser.setSelectedExtensionFilter(new ExtensionFilter("Toy Language soure files", "*.tl"));

    var file = fileChooser.showOpenDialog(_stage);
    var path = file.toPath();

    try {
      var code = Files.readString(path);
      _editor.setText(code);
      _file = Maybe.just(path);

      _stage.setTitle("TL Playground - " + path.getFileName().toString());

      _editor.setDisable(false);
      _save.setDisable(false);
      _run.setDisable(false);
    } catch (IOException e) { }
  }

  void _save() {
    _file.matchDo(
      () -> { },
      path -> {
      try {
        Files.writeString(path, _editor.getText());
        _stage.setTitle("TL Playground - " + path.getFileName().toString());
      } catch (IOException e) { }
    });
  }

  Maybe<Stmt> _compile(String code) {
    try {
      var ast = TLParser.parse(code);
      ast.typeCheck(Map.empty());
      return Maybe.just(ast);
    } catch (UParsecException e) {
      showErrorAlert("Parser error", e.getMessage());
      return Maybe.nothing();
    } catch (TypeCheckerException e) {
      showErrorAlert("Type error", e.getMessage());
      return Maybe.nothing();
    }
  }

  void _run() {
    _save();

    var code = _editor.getText();
    _compile(code).matchDo(
      () -> { },
      ast -> {
        _vbox.setDisable(true);
        runChild(_stage, ExecutionWindow.withAST(ast));
        _vbox.setDisable(false);
      });
  }

  @Override
  public Parent view() {
    return _vbox;
  }
}
