/*
  interpreter.cc -- implement Interpreter

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include <assert.h>
#include "interpreter.hh"

Interpreter::Interpreter()
{
    music_list_i_ =0;
}

Interpreter::~Interpreter()
{
//    assert(!music_list_i_ );
}
