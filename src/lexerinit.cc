/*
  lexerinit.cc -- implement some stuff

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <iostream.h>
#include <strstream.h>
#include "proto.hh"
#include "plist.hh"
#include "lexer.hh"
#include "debug.hh"
#include "main.hh"
#include "sourcefile.hh"
#include "source.hh"

My_flex_lexer *lexer=0;

int
yylex() {
	return lexer->yylex();
}

bool
busy_parsing()
{
    return lexer;	
}
