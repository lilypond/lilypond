/*
  property-iterator.cc -- implement Property_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "property-iterator.hh"
#include "translation-property.hh"
#include "translator-group.hh"

/**
  There is no real processing to a property: just lookup the
  translation unit, and set the property.
  */
void
Property_iterator::do_process_and_next (Moment m)
{
  Translation_property * prop = dynamic_cast<Translation_property *> (music_l_);
  SCM sym = prop->get_mus_property ("symbol");
  if (gh_symbol_p(sym))
    report_to_l ()->set_property (sym, prop->get_mus_property ("value"));
  Music_iterator::do_process_and_next (m);
}

