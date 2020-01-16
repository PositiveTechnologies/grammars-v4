#!/bin/sh

java -jar "$HOME/opt/antlr-4.7.2-complete.jar" ../TypeScriptLexer.g4 ../TypeScriptParser.g4
mv ../*.java ./

CLASSPATH=".:$HOME/opt/antlr-4.7.2-complete.jar:$CLASSPATH" javac *.java

