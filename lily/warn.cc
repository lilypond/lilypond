/*
  warn.cc -- implement warning and error messages. Needs cleanup.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"
#include "debug.hh"
#include "my-lily-lexer.hh"
#include "moment.hh"
#include "timing-translator.hh"
#include "source-file.hh"
#include "source.hh"
#include "main.hh"
#include "input.hh"

ostream &warnout (cerr);
ostream *mlog (&cerr);


