/*
  property-iterator.cc -- implement Property_iterator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "property-iterator.hh"
#include "music.hh"
#include "translator-def.hh"
#include "translator-group.hh"


bool check_grob(Music *mus, SCM sym);

/**
   There is no real processing to a property: just lookup the
   translation unit, and set the property.
*/
void
Property_iterator::process (Moment m)
{
  SCM sym = get_music ()->get_mus_property ("symbol");
  if (gh_symbol_p (sym))
    {
      SCM val = get_music ()->get_mus_property ("value");
      bool ok= true;
      if (val != SCM_EOL)
	ok = type_check_assignment (sym, val, ly_symbol2scm ("translation-type?"));
      if (ok)
	report_to ()->internal_set_property (sym, val);
    }
  Simple_music_iterator::process (m);
}

void
Property_unset_iterator::process (Moment m)
{
  SCM sym = get_music ()->get_mus_property ("symbol");
  type_check_assignment (sym, SCM_EOL, ly_symbol2scm ("translation-type?"));  
  report_to ()->unset_property (sym);

  Simple_music_iterator::process (m);
}


SCM list_p = 0;

bool
check_grob(Music *mus, SCM sym)
{
  if (!list_p)
    {
      list_p = gh_eval_str ("list?");
    }
  
  
  SCM type = scm_object_property (sym, ly_symbol2scm ("translation-type?"));
  bool ok = type == list_p;

  if (!ok)
    {
      mus->origin()->warning (_f("Not a grob name, `%s'." , ly_symbol2string (sym)));
    }
  return  ok;
}

void
Push_property_iterator::process (Moment m)
{
  SCM sym = get_music ()->get_mus_property ("symbol");
  if (check_grob (get_music (), sym))
    {
      SCM eprop = get_music ()->get_mus_property ("grob-property");
      SCM val = get_music ()->get_mus_property ("grob-value");

      if (to_boolean (get_music ()->get_mus_property ("pop-first")))
	Translator_def::apply_pushpop_property (report_to (),
						sym, eprop, SCM_UNDEFINED);

      Translator_def::apply_pushpop_property (report_to (), sym, eprop, val);
    }
  Simple_music_iterator::process (m);
}

void
Pop_property_iterator::process (Moment m)
{
  SCM sym = get_music ()->get_mus_property ("symbol");
  if (check_grob (get_music (), sym))
    {
      SCM eprop = get_music ()->get_mus_property ("grob-property");
      Translator_def::apply_pushpop_property (report_to (), sym, eprop, SCM_UNDEFINED);
    }  
  Simple_music_iterator::process (m);
}


IMPLEMENT_CTOR_CALLBACK (Pop_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Push_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_unset_iterator);
