/*
  lexerinit.cc -- implement some stuff

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <iostream.h>
#include <strstream.h>
#include "proto.hh"
#include "plist.hh"
#include "my-lily-lexer.hh"
#include "debug.hh"
#include "main.hh"
#include "source-file.hh"
#include "source.hh"

My_lily_lexer *lexer=0;

int
yylex() {
	return lexer->yylex();
}

bool
busy_parsing()
{
  return lexer;	
}
