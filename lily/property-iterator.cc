/*
  property-iterator.cc -- implement Property_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "property-iterator.hh"
#include "translation-property.hh"
#include "translator-group.hh"

void
Property_iterator::do_process_and_next (Moment m)
{
  if (property_l()->var_str_.length_i ())
    report_to_l ()->set_property (property_l()->var_str_, property_l()->value_);
  Music_iterator::do_process_and_next (m);
}

IMPLEMENT_IS_TYPE_B1(Property_iterator, Music_iterator);

Translation_property*
Property_iterator::property_l () const
{
  return (Translation_property*) music_l_;
}
