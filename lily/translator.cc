/*
  Translator.cc -- implement Translator

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "translator.hh"
Translator::Translator()
{
    iterator_count_  = 0;
}

IMPLEMENT_STATIC_NAME(Translator);
IMPLEMENT_IS_TYPE_B(Translator);

bool
Translator::try_request(Request*)
{
    return false;
}
