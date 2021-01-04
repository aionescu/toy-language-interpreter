package com.aionescu.tli.view.gui;

import javafx.application.Application;
import javafx.stage.Stage;

public final class GUIBootstrapper extends Application {
  public static void main(String[] args) {
    launch(args);
  }

  @Override
  public void start(Stage stage) {
    new EditorWindow().run(stage);
  }
}
