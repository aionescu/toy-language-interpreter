#!/bin/sh

ARGS="$@"
mvn exec:java -Dexec.args="$ARGS"
