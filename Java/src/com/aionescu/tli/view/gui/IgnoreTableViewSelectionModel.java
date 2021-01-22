package com.aionescu.tli.view.gui;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TablePosition;
import javafx.scene.control.TableView;
import javafx.scene.control.TableView.TableViewSelectionModel;

public final class IgnoreTableViewSelectionModel<T> extends TableViewSelectionModel<T> {
  public IgnoreTableViewSelectionModel(TableView<T> tableView) {
    super(tableView);
  }

  @Override
  @SuppressWarnings("rawtypes")
  public ObservableList<TablePosition> getSelectedCells() {
    return FXCollections.emptyObservableList();
  }

  @Override
  public boolean isSelected(int row, TableColumn<T, ?> column) {
    return false;
  }

  @Override
  public void select(int row, TableColumn<T, ?> column) { }

  @Override
  public void clearAndSelect(int row, TableColumn<T, ?> column) { }

  @Override
  public void clearSelection(int row, TableColumn<T, ?> column) { }

  @Override
  public void selectLeftCell() { }

  @Override
  public void selectRightCell() { }

  @Override
  public void selectAboveCell() { }

  @Override
  public void selectBelowCell() { }
}
