package com.aionescu.tli;

import com.aionescu.tli.repo.SingleStateRepository;

import com.aionescu.tli.controller.Controller;
import com.aionescu.tli.view.CLIView;

public final class Main {
  public static void main(String[] args) {
    var repo = new SingleStateRepository();
    var controller = new Controller(repo);
    var view = new CLIView(controller);

    view.run(args);
  }
}
