package com.aionescu.tli.view.gui;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.stage.Stage;

public final class GUIBootstrapper extends Application {
  public static void main(String[] args) {
    launch(args);
  }

  @Override
  public void start(Stage stage) {
    var childStage = new Stage();
    childStage.initOwner(stage);

    var scene = new Scene(new EditorWindow(childStage).getView());
    scene.getStylesheets().add("file:modena-dark.css");

    childStage.setTitle("TL Playground");
    childStage.setScene(scene);
    childStage.show();
  }
}
