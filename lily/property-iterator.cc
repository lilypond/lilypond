/*
  property-iterator.cc -- implement Property_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "property-iterator.hh"

#include "context-def.hh"
#include "global-context.hh"
#include "international.hh"
#include "music.hh"

bool check_grob (Music *mus, SCM sym);

/**
   There is no real processing to a property: just lookup the
   translation unit, and set the property.
*/
void
Property_iterator::process (Moment m)
{
  SCM sym = get_music ()->get_property ("symbol");
  if (scm_is_symbol (sym))
    {
      SCM val = get_music ()->get_property ("value");
      bool ok = true;
      if (val != SCM_EOL)
	ok = type_check_assignment (sym, val, ly_symbol2scm ("translation-type?"));
      if (ok)
	get_outlet ()->internal_set_property (sym, val);
    }
  Simple_music_iterator::process (m);
}

void
Property_unset_iterator::process (Moment m)
{
  SCM sym = get_music ()->get_property ("symbol");
  type_check_assignment (sym, SCM_EOL, ly_symbol2scm ("translation-type?"));
  get_outlet ()->unset_property (sym);

  Simple_music_iterator::process (m);
}

MAKE_SCHEME_CALLBACK (Property_iterator, once_finalization, 2);
SCM
Property_iterator::once_finalization (SCM translator, SCM music)
{
  Music *m = unsmob_music (music);
  Context *tg
    = dynamic_cast<Context *> (unsmob_context (translator));
  SCM sym = m->get_property ("symbol");

  tg->unset_property (sym);
  return SCM_UNSPECIFIED;
}

void
Property_iterator::do_quit ()
{
  if (to_boolean (get_music ()->get_property ("once")))
    {
      SCM trans = get_outlet ()->self_scm ();
      SCM music = get_music ()->self_scm ();

      Global_context *tg = get_outlet ()->get_global_context ();
      tg->add_finalization (scm_list_n (once_finalization_proc,
					trans, music, SCM_UNDEFINED));
    }
}

bool
check_grob (Music *mus, SCM sym)
{
  bool g = to_boolean (scm_object_property (sym, ly_symbol2scm ("is-grob?")));

  if (!g)
    mus->origin ()->warning (_f ("not a grob name, `%s'",
				 ly_symbol2string (sym)));

  return g;
}

SCM
get_property_path (Music *m)
{
  SCM grob_property_path = m->get_property ("grob-property-path");

  SCM eprop = m->get_property ("grob-property");
  if (scm_is_symbol (eprop))
    {
      grob_property_path = scm_list_1 (eprop);
    }

  return grob_property_path;
}

void
Push_property_iterator::process (Moment m)
{
  SCM sym = get_music ()->get_property ("symbol");
  if (check_grob (get_music (), sym))
    {
      SCM grob_property_path = get_property_path (get_music ());
      SCM val = get_music ()->get_property ("grob-value");

      if (to_boolean (get_music ()->get_property ("pop-first"))
	  && !to_boolean (get_music ()->get_property ("once")))
	
	execute_general_pushpop_property (get_outlet (), sym, grob_property_path, SCM_UNDEFINED);

      execute_general_pushpop_property (get_outlet (), sym, grob_property_path, val);
    }
  Simple_music_iterator::process (m);
}

MAKE_SCHEME_CALLBACK (Push_property_iterator, once_finalization, 2);
SCM
Push_property_iterator::once_finalization (SCM trans, SCM music)
{
  Music *mus = unsmob_music (music);
  Context *tg = dynamic_cast<Context *> (unsmob_context (trans));

  SCM sym = mus->get_property ("symbol");
  if (check_grob (mus, sym))
    {
      SCM grob_property_path = get_property_path (mus);

      execute_general_pushpop_property (tg, sym, grob_property_path, SCM_UNDEFINED);
    }
  return SCM_UNSPECIFIED;
}

void
Push_property_iterator::do_quit ()
{
  if (to_boolean (get_music ()->get_property ("once")))
    {
      SCM trans = get_outlet ()->self_scm ();
      SCM music = get_music ()->self_scm ();

      Global_context *tg = get_outlet ()->get_global_context ();
      tg->add_finalization (scm_list_n (once_finalization_proc,
					trans, music, SCM_UNDEFINED));
    }
}

void
Pop_property_iterator::process (Moment m)
{
  SCM sym = get_music ()->get_property ("symbol");

  if (check_grob (get_music (), sym))
    {
      SCM grob_property_path = get_property_path (get_music ());
      execute_general_pushpop_property (get_outlet (), sym, grob_property_path, SCM_UNDEFINED);
    }
  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Pop_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Push_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_unset_iterator);

