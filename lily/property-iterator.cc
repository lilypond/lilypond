/*
  property-iterator.cc -- implement Property_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "property-iterator.hh"
#include "translation-property.hh"
#include "translator-group.hh"

Property_iterator::Property_iterator (Translation_property *prop_l)
{
  property_l_ = prop_l;
}

void
Property_iterator::process_and_next (Moment m)
{
  if (property_l_->var_str_.length_i ())
    report_to_l ()->set_property (property_l_->var_str_, property_l_->value_);
  Music_iterator::process_and_next (m);
}

IMPLEMENT_IS_TYPE_B1(Property_iterator, Music_iterator);
