package com.aionescu.tli;

import com.aionescu.tli.repo.SingleStateRepository;

import java.util.Arrays;

import com.aionescu.tli.controller.Controller;
import com.aionescu.tli.view.CLIView;

public final class Main {
  public static void main(String[] args) {
    Arrays.stream(args).forEach(System.out::println);
    var repo = new SingleStateRepository();
    var controller = new Controller(repo);
    var view = new CLIView(controller);

    view.run();
  }
}
