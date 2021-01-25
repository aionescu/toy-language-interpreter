package com.aionescu.tli.view.gui;

import javafx.collections.FXCollections;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.stage.Stage;

import com.aionescu.tli.ast.Ident;
import com.aionescu.tli.ast.prog.GlobalState;
import com.aionescu.tli.ast.prog.GCStats;
import com.aionescu.tli.ast.stmt.Stmt;
import com.aionescu.tli.ast.val.VStr;
import com.aionescu.tli.ast.val.Val;
import com.aionescu.tli.controller.Controller;
import com.aionescu.tli.exn.eval.EvalException;
import com.aionescu.tli.utils.Pair;
import com.aionescu.tli.utils.data.list.List;

import static com.aionescu.tli.view.gui.GUIWindow.*;

public final class ExecutionWindow implements GUIWindow {
  private final Controller _controller;

  private final Stage _stage;

  private final Button _runOneStep, _runAllSteps;
  private final TextField _threadStateCount;
  private final TableView<Pair<String, Val>> _heap;
  private final Label _heapLabel;
  private final ListView<Val> _out;
  private final ListView<Pair<String, List<Val>>> _files;
  private final ListView<Integer> _threadStateIDs;
  private final TableView<Pair<Ident, Val>> _sym;
  private final ListView<Stmt> _toDo;
  private final VBox _vbox;

  public ExecutionWindow(Controller controller, Stage stage) {
    _controller = controller;

    _stage = stage;
    _stage.setTitle("Program Execution");

    _threadStateCount = new TextField();
    _threadStateCount.setEditable(false);
    _threadStateCount.setPrefSize(200, 50);
    _threadStateCount.setFont(new Font("Fira Code Regular", 14));

    _heapLabel = mkLabel("Heap:");
    _heap = mkPairTableView("Address", "Value");
    _out = mkListView(false);
    _files = mkListView(false);

    _threadStateIDs = mkListView(true);
    _threadStateIDs.getSelectionModel().selectedItemProperty().addListener((obs, oldVal, newVal) -> {
      if (newVal != null)
        _populateThreadLocalWidgets(newVal);
    });

    _sym = mkPairTableView("Identifier", "Value");
    _toDo = mkListView(false);

    _runOneStep = mkButton("Run one step", () -> _runOneStep());
    _runAllSteps = mkButton("Run all steps", () -> _runAllSteps());

    _vbox = new VBox(
      new HBox(_runOneStep, _runAllSteps, _threadStateCount),
      _heapLabel,
      _heap,
      new HBox(
        new VBox(mkLabel("Output:"), _out),
        new VBox(mkLabel("Files:"), _files),
        new VBox(mkLabel("Threads:"), _threadStateIDs)
      ),
      mkLabel("Symbol table:"),
      _sym,
      mkLabel("Execution stack:"),
      _toDo
    );

    _populateWidgets();
  }

  void _populateThreadLocalWidgets(Integer selectedID) {
    if (selectedID == null) {
      _toDo.setItems(FXCollections.emptyObservableList());
      _sym.setItems(FXCollections.emptyObservableList());

      _sym.setDisable(true);
      _toDo.setDisable(true);

      return;
    }

    int threadID = selectedID;
    var maybeThread = _global().threads.find(t -> t.id == threadID);

    maybeThread.matchDo(
      () -> { },
      thread -> {
        var toDo = thread.toDo.toList().toObservable();
        var sym = thread.sym.toList().toObservable();

        _toDo.setItems(toDo);
        _sym.setItems(sym);

        _sym.setDisable(false);
        _toDo.setDisable(false);
      });
  }

  void _populateWidgets() {
    if (_controller.done()) {
      _runOneStep.setDisable(true);
      _runAllSteps.setDisable(true);
    }

    var global = _global();

    _threadStateCount.setText("ThreadState Count: " + String.valueOf(global.threads.length()));

    _heapLabel.setText("Heap: " + global.gcStats);

    var heap = global.heap.toList().map(Pair.first(GCStats::showHex)).toObservable();
    _heap.setItems(heap);

    var out = global.out.reverse().toObservable();
    _out.setItems(out);

    var files = global.open.toList().map(Pair.first(VStr::escapeString)).toObservable();
    _files.setItems(files);

    var threadIDs = global.threads.map(t -> t.id).toObservable();
    _threadStateIDs.setItems(threadIDs);
    _populateThreadLocalWidgets(_threadStateIDs.getSelectionModel().getSelectedItem());
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
  public Parent view() {
    return _vbox;
  }
}
