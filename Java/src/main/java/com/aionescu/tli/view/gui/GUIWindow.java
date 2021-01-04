package com.aionescu.tli.view.gui;

import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.stage.Stage;

public interface GUIWindow {
  void setStage(Stage stage);
  String title();
  Parent view();

  default void run(Stage parentStage) {
    var childStage = new Stage();
    childStage.initOwner(parentStage);
    setStage(childStage);

    var scene = new Scene(view());
    scene.getStylesheets().add("file:modena-dark.css");

    childStage.setTitle(title());
    childStage.setScene(scene);
    childStage.showAndWait();
  }

  default void showErrorAlert(String title, String content) {
    var alert = new Alert(AlertType.ERROR);
    alert.getDialogPane().getStylesheets().add("file:modena-dark.css");

    alert.setTitle(title);
    alert.setHeaderText(null);
    alert.setContentText(content);

    alert.showAndWait();
  }
}
