/*
  span-bar-engraver.cc -- implement Span_bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "base-span-bar-engraver.hh"

class Span_bar_engraver : public Base_span_bar_engraver
{
public:
  Span_bar_engraver ();
  VIRTUAL_COPY_CONS (Translator);
};

ADD_THIS_TRANSLATOR (Span_bar_engraver);

Span_bar_engraver::Span_bar_engraver ()
{
  use_priority_b_ = false;
}
