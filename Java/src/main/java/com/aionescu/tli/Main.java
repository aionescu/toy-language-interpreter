package com.aionescu.tli;

import com.aionescu.tli.controller.Controller;
import com.aionescu.tli.repo.SingleStateRepository;
import com.aionescu.tli.view.gui.GUIBootstrapper;
import com.aionescu.tli.view.tui.TUIView;

public final class Main {
  public static void main(String[] args) {
    if (args.length > 0 && args[0].equals("--gui"))
      GUIBootstrapper.runGUI(args);
    else {
      var repo = new SingleStateRepository();
      var controller = new Controller(repo);
      var view = new TUIView(controller);

      view.run(args);
    }
  }
}
