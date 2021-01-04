package com.aionescu.tli.utils;

import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.text.Font;
import javafx.util.Callback;

public final class FXUtils {
  public static <T, C> Callback<TableColumn<T, C>, TableCell<T, C>> tableViewSetFont() {
    return p -> new TableCell<T, C>() {
      @Override
      public void updateItem(C item, boolean empty) {
        super.updateItem(item, empty);
        setFont(new Font("Fira Code Regular", 16));
        setText(empty ? "" : item.toString());
      }
    };
  }

  public static <A> Callback<ListView<A>, ListCell<A>> listViewSetFont() {
    return p -> new ListCell<A>() {
      @Override
      protected void updateItem(A item, boolean empty) {
        super.updateItem(item, empty);
        setFont(new Font("Fira Code Regular", 16));
        setText(empty ? "" : item.toString());
      }
    };
  }
}
