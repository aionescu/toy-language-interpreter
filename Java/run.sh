#!/bin/sh

ARGS="$@"
mvn clean compile exec:java -Dexec.args="$ARGS"
