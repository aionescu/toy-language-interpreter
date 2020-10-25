#!/bin/sh

ARGS="$@"
mvn -q exec:java -Dexec.args="$ARGS"
