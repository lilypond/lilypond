#include "base-span-bar-engraver.hh"


class Span_bar_engraver : public Base_span_bar_engraver
{
public:
  Span_bar_engraver ();
  VIRTUAL_COPY_CONS (Translator);
};


Span_bar_engraver::Span_bar_engraver ()
{
  use_priority_b_ = false;
}

ADD_THIS_TRANSLATOR(Span_bar_engraver);
