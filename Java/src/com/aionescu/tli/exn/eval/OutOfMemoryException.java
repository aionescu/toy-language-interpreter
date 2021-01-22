package com.aionescu.tli.exn.eval;

public final class OutOfMemoryException extends EvalException {
  private final static long serialVersionUID = 1;

  private final int _heapSize;

  public OutOfMemoryException(int heapSize) {
    super();

    _heapSize = heapSize;
  }

  @Override
  public String getMessage() {
    return String.format("Out of memory (heap size: %d).", _heapSize);
  }
}
