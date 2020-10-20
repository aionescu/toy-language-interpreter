package utils;

import java.util.function.BinaryOperator;

import utils.uparsec.Parser;

public final class Main {
  private static String parenL(String a, String b) {
    return "(" + a + ")" + b;
  }

  private static String parenR(String a, String b) {
    return a + "(" + b + ")";
  }

  public static void main(String[] args) {
    Parser<BinaryOperator<String>> op = Parser.ch('+').map_(Main::parenL);
    var p = Parser.anyChar().map(c -> "" + c);

    var p2 = p.chainl1(op);

    p2.run("a+b+c+d+ef").matchDo(
      () -> System.out.println("Failed"),
      (r, s) -> {
        System.out.println("Success");
        System.out.println(r);
        System.out.println(s);
      });
  }
}
