package com.aionescu.tli.view.gui;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.aionescu.tli.ast.stmt.Stmt;
import com.aionescu.tli.exn.typeck.TypeCheckerException;
import com.aionescu.tli.parser.TLParser;
import com.aionescu.tli.utils.control.Maybe;
import com.aionescu.tli.utils.data.map.Map;
import com.aionescu.tli.utils.uparsec.exn.UParsecException;

import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.stage.FileChooser;
import javafx.stage.Stage;
import javafx.stage.FileChooser.ExtensionFilter;

public final class EditorWindow implements GUIWindow {
  final Stage _stage;
  final Button _open, _save, _run;
  final TextArea _editor;
  Maybe<Path> _file = Maybe.nothing();
  final VBox _vbox;

  public EditorWindow(Stage stage) {
    _stage = stage;

    _open = new Button("Open");
    _open.setOnAction(e -> _open());

    _save = new Button("Save");
    _save.setOnAction(e -> _save());

    _run = new Button("Run");
    _run.setOnAction(e -> _run());

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

  void _showError(String type, String error) {
    var alert = new Alert(AlertType.ERROR);
    alert.getDialogPane().getStylesheets().add("file:modena-dark.css");

    alert.setTitle(type + " error");
    alert.setHeaderText(null);
    alert.setContentText(error);

    alert.showAndWait();
  }

  Maybe<Stmt> _compile(String code) {
    try {
      var ast = TLParser.parse(code);
      ast.typeCheck(Map.empty());
      return Maybe.just(ast);
    } catch (UParsecException e) {
      _showError("Parser", e.getMessage());
      return Maybe.nothing();
    } catch (TypeCheckerException e) {
      _showError("Type", e.getMessage());
      return Maybe.nothing();
    }
  }

  void _run() {
    _save();

    var code = _editor.getText();
    _compile(code).matchDo(
      () -> { },
      ast -> {
        var stage = new Stage();
        stage.initOwner(_stage);
        stage.setTitle("Program Execution");

        var scene = new Scene(new ExecutionWindow(stage, ast).getView());
        scene.getStylesheets().add("file:modena-dark.css");
        stage.setScene(scene);

        _vbox.setDisable(true);
        stage.showAndWait();
        _vbox.setDisable(false);
      });
  }

  @Override
  public Parent getView() {
    return _vbox;
  }
}
