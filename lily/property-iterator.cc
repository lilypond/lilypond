/*
  property-iterator.cc -- implement Property_iterator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "property-iterator.hh"
#include "music.hh"
#include "context-def.hh"
#include "global-context.hh"

bool check_grob (Music *mus, SCM sym);

/**
   There is no real processing to a property: just lookup the
   translation unit, and set the property.
*/
void
Property_iterator::process (Moment m)
{
  SCM sym = get_music ()->get_property ("symbol");
  if (ly_c_symbol_p (sym))
    {
      SCM val = get_music ()->get_property ("value");
      bool ok= true;
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

MAKE_SCHEME_CALLBACK (Property_iterator,once_finalization, 2);
SCM
Property_iterator::once_finalization (SCM translator, SCM music )
{
  Music * m = unsmob_music (music);
  Context * tg
    = dynamic_cast<Context *> (unsmob_context (translator));
  SCM sym = m->get_property ("symbol");

  tg->unset_property (sym);
  return SCM_UNSPECIFIED;
}

void
Property_iterator::do_quit ()
{
  if (to_boolean (get_music ()->get_property  ("once")))
    {
      SCM trans = get_outlet ()->self_scm ();
      SCM music = get_music ()->self_scm ();

      Global_context * tg = get_outlet ()->get_global_context ();
      tg->add_finalization (scm_list_n (once_finalization_proc,
					trans, music, SCM_UNDEFINED));
    }
}


SCM list_p = 0;

/*
  This is a rather crude check: we merely check if the translator
  property is a list.
 */
bool
check_grob (Music *mus, SCM sym)
{
  if (!list_p)
    {
      list_p = scm_c_eval_string ("list?");
    }
  
  
  SCM type = scm_object_property (sym, ly_symbol2scm ("translation-type?"));
  bool ok = type == list_p;

  if (!ok)
    {
      mus->origin ()->warning (_f ("Not a grob name, `%s'." , ly_symbol2string (sym)));
    }
  return  ok;
}

void
Push_property_iterator::process (Moment m)
{
  SCM sym = get_music ()->get_property ("symbol");
  if (check_grob (get_music (), sym))
    {
      SCM eprop = get_music ()->get_property ("grob-property");
      SCM val = get_music ()->get_property ("grob-value");

      if (to_boolean (get_music ()->get_property ("pop-first"))
	  && !to_boolean (get_music ()->get_property ("once")))
	execute_pushpop_property (get_outlet (), sym, eprop, SCM_UNDEFINED);

      execute_pushpop_property (get_outlet (), sym, eprop, val);
    }
  Simple_music_iterator::process (m);
}

MAKE_SCHEME_CALLBACK (Push_property_iterator,once_finalization, 2);
SCM
Push_property_iterator::once_finalization (SCM trans, SCM music)
{
  Music * mus = unsmob_music (music);
  Context * tg = dynamic_cast<Context *> (unsmob_context (trans));
    
  SCM sym = mus->get_property ("symbol");
  if (check_grob (mus, sym))
    {
      SCM eprop = mus->get_property ("grob-property");
  
      execute_pushpop_property (tg, sym, eprop, SCM_UNDEFINED);
    }
  return SCM_UNSPECIFIED;
}

void
Push_property_iterator::do_quit ()
{
  if (to_boolean (get_music ()->get_property  ("once")))
    {
      SCM trans = get_outlet ()->self_scm ();
      SCM music = get_music ()->self_scm ();

      Global_context * tg=  get_outlet ()->get_global_context ();
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
      SCM eprop = get_music ()->get_property ("grob-property");
execute_pushpop_property (get_outlet (), sym, eprop, SCM_UNDEFINED);
    }
  Simple_music_iterator::process (m);
}



IMPLEMENT_CTOR_CALLBACK (Pop_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Push_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_unset_iterator);

