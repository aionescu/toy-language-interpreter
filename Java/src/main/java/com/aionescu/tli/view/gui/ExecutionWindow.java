package com.aionescu.tli.view.gui;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.GlobalState;
import com.aionescu.tli.ast.prog.GCStats;
import com.aionescu.tli.ast.stmt.Stmt;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.controller.Controller;
import com.aionescu.tli.repo.SingleStateRepository;
import com.aionescu.tli.utils.FXUtils;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.data.list.List;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.collections.FXCollections;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

public final class ExecutionWindow implements GUIWindow {
  final Stmt _ast;
  final Controller _controller;

  final Stage _stage;
  final VBox _vbox;

  final TextField _threadStateCount;
  final TableView<Pair<String, Val>> _heap;
  final ListView<Val> _out;
  final ListView<Pair<String, List<Val>>> _files;
  final ListView<Integer> _threadStateIDs;
  final TableView<Pair<Ident, Val>> _sym;
  final ListView<Stmt> _toDo;

  final Button _runOneStep;
  final Button _runAllSteps;

  public ExecutionWindow(Stage stage, Stmt ast) {
    _stage = stage;
    _ast = ast;

    _controller = new Controller(new SingleStateRepository());
    _controller.setState(GlobalState.initial(_ast));

    _threadStateCount = new TextField();

    _heap = new TableView<>();
    _heap.setSelectionModel(new TableViewNoSelectionModel<>(_heap));
    _heap.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);

    var addr = new TableColumn<Pair<String, Val>, String>("Address");
    addr.setCellValueFactory(v -> new ReadOnlyObjectWrapper<>(v.getValue().fst));
    addr.setCellFactory(FXUtils.tableViewSetFont());

    var heapVal = new TableColumn<Pair<String, Val>, Val>("Value");
    heapVal.setCellValueFactory(v -> new ReadOnlyObjectWrapper<>(v.getValue().snd));
    heapVal.setCellFactory(FXUtils.tableViewSetFont());

    _heap.getColumns().add(addr);
    _heap.getColumns().add(heapVal);

    _out = new ListView<>();
    _out.setSelectionModel(new NoSelectionModel<>());
    _out.setCellFactory(FXUtils.listViewSetFont());

    _files = new ListView<>();
    _files.setSelectionModel(new NoSelectionModel<>());
    _files.setCellFactory(FXUtils.listViewSetFont());

    _threadStateIDs = new ListView<>();
    _threadStateIDs.setCellFactory(FXUtils.listViewSetFont());

    _threadStateIDs.getSelectionModel().selectedItemProperty().addListener((obs, oldVal, newVal) -> {
      if (newVal != null)
        _populateThreadLocalWidgets(newVal);
    });

    _sym = new TableView<>();
    _sym.setSelectionModel(new TableViewNoSelectionModel<>(_sym));
    _sym.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);

    var ident = new TableColumn<Pair<Ident, Val>, Ident>("Identifier");
    ident.setCellValueFactory(v -> new ReadOnlyObjectWrapper<>(v.getValue().fst));
    ident.setCellFactory(FXUtils.tableViewSetFont());

    var symVal = new TableColumn<Pair<Ident, Val>, Val>("Value");
    symVal.setCellValueFactory(v -> new ReadOnlyObjectWrapper<>(v.getValue().snd));
    symVal.setCellFactory(FXUtils.tableViewSetFont());

    _sym.getColumns().add(ident);
    _sym.getColumns().add(symVal);

    _toDo = new ListView<>();
    _toDo.setSelectionModel(new NoSelectionModel<>());
    _toDo.setCellFactory(FXUtils.listViewSetFont());

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
        var toDo = thread.toDo.toList().bind(Stmt::explode).toObservable();
        var sym = thread.sym.toList().toObservable();

        _toDo.setItems(toDo);
        _sym.setItems(sym);
      });
  }

  void _populateWidgets() {
    var global = _global();

    _threadStateCount.setText("ThreadState Count: " + String.valueOf(global.threads.length()));

    var heap = global.heap.toList().map(Pair.match((a, b) -> Pair.of(GCStats.showHex(a), b))).toObservable();
    _heap.setItems(heap);

    var out = global.out.reverse().toObservable();
    _out.setItems(out);

    var files = global.open.toList().toObservable();
    _files.setItems(files);

    var threadIDs = global.threads.map(t -> t.id).toObservable();
    _threadStateIDs.setItems(threadIDs);
  }

  GlobalState _global() {
    return _controller.state().get();
  }

  void _runOneStep() {
    _controller.oneStep();

    if (_controller.done()) {
      _runOneStep.setDisable(true);
      _runAllSteps.setDisable(true);
    }

    _populateWidgets();
  }

  void _runAllSteps() {
    while (!_controller.done())
      _controller.oneStep();

    _runOneStep.setDisable(true);
    _runAllSteps.setDisable(true);

    _populateWidgets();
  }

  @Override
  public Parent getView() {
    return _vbox;
  }
}
