#!/bin/bash
flex --outfile lexer.cpp --header-file=lexer.hpp lexer.l
`which bison` -d -Wcounterexamples --output yacc_pascal.cpp parser.y