/*
  property-iterator.cc -- implement Property_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  Translation_property const * prop = dynamic_cast<Translation_property const*> (music_l_);
  if (prop->var_str_.length_i ())
    report_to_l ()->set_property (prop->var_str_, prop->value_);
  Music_iterator::do_process_and_next (m);
}

