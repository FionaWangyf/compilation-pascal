#!/bin/bash
flex --outfile lexer.cpp --header-file=lexer.hpp lexer.l
`which bison` -d -Wcounterexamples --output parser.cpp parser.y