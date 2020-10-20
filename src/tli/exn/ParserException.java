package tli.exn;

public final class ParserException extends TLIException {
  private final static long serialVersionUID = 1;

  @Override
  public String getMessage() {
    return "Parsing failed.";
  }
}
