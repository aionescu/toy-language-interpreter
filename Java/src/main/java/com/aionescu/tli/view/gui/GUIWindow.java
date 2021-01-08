package com.aionescu.tli.view.gui;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.text.Font;
import javafx.stage.Stage;
import javafx.util.Callback;

import com.aionescu.tli.utils.Pair;

public interface GUIWindow {
  void setStage(Stage stage);
  String title();
  Parent view();

  static void showErrorAlert(String title, String content) {
    var alert = new Alert(AlertType.ERROR);
    alert.getDialogPane().getStylesheets().add("file:modena-dark.css");

    alert.setTitle(title);
    alert.setHeaderText(null);
    alert.setContentText(content);

    alert.showAndWait();
  }

  static Label mkLabel(String text) {
    var label = new Label(text);
    label.setFont(new Font("Fira Code Regular", 14));

    return label;
  }

  static Button mkButton(String text, Runnable handler) {
    var button = new Button(text);
    button.setFont(new Font("Fira Code Regular", 14));
    button.setOnAction(e -> handler.run());

    return button;
  }

  static <A> ListView<A> mkListView(boolean enableSelection) {
    var list = new ListView<A>();

    if (!enableSelection)
    list.setSelectionModel(new IgnoreSelectionModel<>());

    list.setCellFactory(p -> new ListCell<A>() {
      @Override
      protected void updateItem(A item, boolean empty) {
        super.updateItem(item, empty);
        setFont(new Font("Fira Code Regular", 16));
        setText(empty ? "" : item.toString());
      }
    });

    return list;
  }

  private static <T, C> Callback<TableColumn<T, C>, TableCell<T, C>> _tableViewCellFactory() {
    return p -> new TableCell<T, C>() {
      @Override
      public void updateItem(C item, boolean empty) {
        super.updateItem(item, empty);
        setFont(new Font("Fira Code Regular", 16));
        setText(empty ? "" : item.toString());
      }
    };
  }

  static <A, B> TableView<Pair<A, B>> mkTableView(String fstName, String sndName) {
    var table = new TableView<Pair<A, B>>();

    table.setSelectionModel(new IgnoreTableViewSelectionModel<>(table));
    table.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);

    var fst = new TableColumn<Pair<A, B>, A>(fstName);
    fst.setCellValueFactory(v -> new ReadOnlyObjectWrapper<>(v.getValue().fst));
    fst.setCellFactory(_tableViewCellFactory());

    var snd = new TableColumn<Pair<A, B>, B>(sndName);
    snd.setCellValueFactory(v -> new ReadOnlyObjectWrapper<>(v.getValue().snd));
    snd.setCellFactory(_tableViewCellFactory());

    table.getColumns().add(fst);
    table.getColumns().add(snd);

    return table;
  }

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
}
