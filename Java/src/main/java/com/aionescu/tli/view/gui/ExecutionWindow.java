package com.aionescu.tli.view.gui;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.GlobalState;
import com.aionescu.tli.ast.prog.GCStats;
import com.aionescu.tli.ast.stmt.Stmt;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.controller.Controller;
import com.aionescu.tli.exn.eval.EvalException;
import com.aionescu.tli.repo.SingleStateRepository;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.data.list.List;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.collections.FXCollections;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.stage.Stage;
import javafx.util.Callback;

public final class ExecutionWindow implements GUIWindow {
  Controller _controller;

  Stage _stage;
  VBox _vbox;

  TextField _threadStateCount;
  TableView<Pair<String, Val>> _heap;
  ListView<Val> _out;
  ListView<Pair<String, List<Val>>> _files;
  ListView<Integer> _threadStateIDs;
  TableView<Pair<Ident, Val>> _sym;
  ListView<Stmt> _toDo;

  Button _runOneStep, _runAllSteps;

  public ExecutionWindow(Stmt ast) {
    _controller = new Controller(new SingleStateRepository());
    _controller.setState(GlobalState.initialExploded(ast));
  }

  static <A> ListView<A> _mkListView(boolean enableSelection) {
    var list = new ListView<A>();

    if (!enableSelection)
    list.setSelectionModel(new NoSelectionModel<>());

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

  static <T, C> Callback<TableColumn<T, C>, TableCell<T, C>> _tableViewCellFactory() {
    return p -> new TableCell<T, C>() {
      @Override
      public void updateItem(C item, boolean empty) {
        super.updateItem(item, empty);
        setFont(new Font("Fira Code Regular", 16));
        setText(empty ? "" : item.toString());
      }
    };
  }

  static <A, B> TableView<Pair<A, B>> _mkTableView(String fstName, String sndName) {
    var table = new TableView<Pair<A, B>>();

    table.setSelectionModel(new TableViewNoSelectionModel<>(table));
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

  void _populateThreadLocalWidgets(Integer threadID_) {
    if (threadID_ == null) {
      _toDo.setItems(FXCollections.emptyObservableList());
      _sym.setItems(FXCollections.emptyObservableList());
    }

    int threadID = threadID_;

    var maybeThread = _global().threads.find(t -> t.id == threadID);

    maybeThread.matchDo(
      () -> { },
      thread -> {
        var toDo = thread.toDo.toList().toObservable();
        var sym = thread.sym.toList().toObservable();

        _toDo.setItems(toDo);
        _sym.setItems(sym);
      });
  }

  void _populateWidgets() {
    if (_controller.done()) {
      _runOneStep.setDisable(true);
      _runAllSteps.setDisable(true);
    }

    var global = _global();

    _threadStateCount.setText("ThreadState Count: " + String.valueOf(global.threads.length()));

    var heap = global.heap.toList().map(Pair.first(GCStats::showHex)).toObservable();
    _heap.setItems(heap);

    var out = global.out.reverse().toObservable();
    _out.setItems(out);

    var files = global.open.toList().toObservable();
    _files.setItems(files);

    var threadIDs = global.threads.map(t -> t.id).toObservable();
    _threadStateIDs.setItems(threadIDs);

    var selected = _threadStateIDs.getSelectionModel().getSelectedItem();
    if (selected == null) {
      _sym.setItems(FXCollections.emptyObservableList());
      _toDo.setItems(FXCollections.emptyObservableList());
    }
  }

  GlobalState _global() {
    return _controller.state().get();
  }

  void _showEvalError(EvalException e) {
    showErrorAlert("Evaluation error", e.getMessage());
    _controller.setState(GlobalState.emptyRef());
    _populateWidgets();
  }

  void _runOneStep() {
    try {
      _controller.oneStep();
      _populateWidgets();
    } catch (EvalException e) {
      _showEvalError(e);
    }
  }

  void _runAllSteps() {
    try {
      while (!_controller.done())
        _controller.oneStep();
    } catch (EvalException e) {
      _showEvalError(e);
    }

    _populateWidgets();
  }

  @Override
  public void setStage(Stage stage) {
    _stage = stage;

    _threadStateCount = new TextField();
    _threadStateCount.setEditable(false);

    _heap = _mkTableView("Address", "Value");
    _out = _mkListView(false);
    _files = _mkListView(false);

    _threadStateIDs = _mkListView(true);
    _threadStateIDs.getSelectionModel().selectedItemProperty().addListener((obs, oldVal, newVal) -> {
      if (newVal != null)
        _populateThreadLocalWidgets(newVal);
    });

    _sym = _mkTableView("Identifier", "Value");

    _toDo = _mkListView(false);

    _runOneStep = new Button("Run one step");
    _runOneStep.setOnAction(e -> _runOneStep());

    _runAllSteps = new Button("Run all steps");
    _runAllSteps.setOnAction(e -> _runAllSteps());

    _vbox = new VBox(
      new HBox(_runOneStep, _runAllSteps, _threadStateCount),
      new Label("Heap:"),
      _heap,
      new HBox(
        new VBox(new Label("Output:"), _out),
        new VBox(new Label("Files:"), _files),
        new VBox(new Label("Threads:"), _threadStateIDs)
      ),
      new Label("Symbol table:"),
      _sym,
      new Label("Execution stack:"),
      _toDo
    );

    _populateWidgets();
  }

  @Override
  public String title() {
    return "Program execution";
  }

  @Override
  public Parent view() {
    return _vbox;
  }
}
