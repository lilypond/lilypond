/*
  property-iterator.cc -- implement Property_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "property-iterator.hh"
#include "music.hh"
#include "translator-def.hh"
#include "translator-group.hh"

/**
  There is no real processing to a property: just lookup the
  translation unit, and set the property.
  */
void
Property_iterator::process (Moment m)
{
  SCM sym = music_l_->get_mus_property ("symbol");
  if (gh_symbol_p(sym))
    {
      SCM val = music_l_->get_mus_property ("value");
      bool ok= true;
      if (val != SCM_EOL)
	ok = type_check_assignment (val, sym, ly_symbol2scm ("translation-type?"));
      if (ok)
	report_to_l ()->set_property (sym, val);
    }
  Simple_music_iterator::process (m);
}

void
Push_property_iterator::process (Moment m)
{
  SCM syms = music_l_->get_mus_property ("symbols");
  SCM eprop = music_l_->get_mus_property ("grob-property");
  SCM val = music_l_->get_mus_property ("grob-value");

  if (to_boolean (music_l_->get_mus_property ( "pop-first")))
    Translator_def::apply_pushpop_property (report_to_l (),
					    syms, eprop, SCM_UNDEFINED);

  Translator_def::apply_pushpop_property (report_to_l (), syms, eprop, val);
  
  Simple_music_iterator::process (m);
}

void
Pop_property_iterator::process (Moment m)
{
  SCM syms = music_l_->get_mus_property ("symbols");
  SCM eprop = music_l_->get_mus_property ("grob-property");
  Translator_def::apply_pushpop_property (report_to_l (), syms, eprop, SCM_UNDEFINED);
  
  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK(Pop_property_iterator);
IMPLEMENT_CTOR_CALLBACK(Push_property_iterator);
IMPLEMENT_CTOR_CALLBACK(Property_iterator);
