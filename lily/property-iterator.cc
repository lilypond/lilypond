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
  SCM sym = music_l_->get_mus_property ("symbol");
  if (gh_symbol_p(sym))
    report_to_l ()->set_property (sym, music_l_->get_mus_property ("value"));
  Music_iterator::do_process_and_next (m);
}


void
Push_property_iterator::do_process_and_next (Moment m)
{
  SCM syms = music_l_->get_mus_property ("symbols");
  SCM eprop = music_l_->get_mus_property ("element-property");
  SCM val = music_l_->get_mus_property ("element-value");

  Translator_group_initializer::apply_pushpop_property (report_to_l (), syms,eprop, val);
  
  Music_iterator::do_process_and_next (m);
}

void
Pop_property_iterator::do_process_and_next (Moment m)
{
  SCM syms = music_l_->get_mus_property ("symbols");
  SCM eprop = music_l_->get_mus_property ("element-property");
  Translator_group_initializer::apply_pushpop_property (report_to_l (), syms, eprop, SCM_UNDEFINED);
  
  Music_iterator::do_process_and_next (m);
}
