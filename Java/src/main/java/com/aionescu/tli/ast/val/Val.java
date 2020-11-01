package com.aionescu.tli.ast.val;

import com.aionescu.tli.ast.type.Type;

public interface Val extends Comparable<Val> {
  Type type();
}
