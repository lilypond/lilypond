/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
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
  send_stream_event (get_outlet (), "SetProperty", get_music ()->origin (),
		     ly_symbol2scm ("symbol"), get_music ()->get_property ("symbol"),
		     ly_symbol2scm ("value"), get_music ()->get_property ("value"));
  
  Simple_music_iterator::process (m);
}

void
Property_unset_iterator::process (Moment m)
{
  SCM sym = get_music ()->get_property ("symbol");
  send_stream_event (get_outlet (), "UnsetProperty", get_music ()->origin (),
		     ly_symbol2scm ("symbol"), sym);

  Simple_music_iterator::process (m);
}

MAKE_SCHEME_CALLBACK (Property_iterator, once_finalization, 2);
SCM
Property_iterator::once_finalization (SCM ctx, SCM music)
{
  Music *m = unsmob_music (music);
  Context *c = unsmob_context (ctx);

  send_stream_event (c, "UnsetProperty", m->origin (),
		     ly_symbol2scm ("symbol"), m->get_property ("symbol"));
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
	send_stream_event (get_outlet (), "Revert", get_music ()->origin (),
			   ly_symbol2scm ("symbol"), sym,
			   ly_symbol2scm ("property-path"), grob_property_path);
			
      send_stream_event (get_outlet (), "Override", get_music ()->origin (),
			 ly_symbol2scm ("symbol"), sym,
			 ly_symbol2scm ("property-path"), grob_property_path,
			 ly_symbol2scm ("value"), val);
    }
  Simple_music_iterator::process (m);
}

MAKE_SCHEME_CALLBACK (Push_property_iterator, once_finalization, 2);
SCM
Push_property_iterator::once_finalization (SCM ctx, SCM music)
{
  Music *mus = unsmob_music (music);
  Context *c = unsmob_context (ctx);

  SCM sym = mus->get_property ("symbol");
  if (check_grob (mus, sym))
    {
      SCM grob_property_path = get_property_path (mus);

      send_stream_event (c, "Revert", mus->origin (),
			 ly_symbol2scm ("symbol"), sym,
			 ly_symbol2scm ("property-path"), grob_property_path);
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

      send_stream_event (get_outlet (), "Revert", get_music ()->origin (),
			 ly_symbol2scm ("symbol"), sym,
			 ly_symbol2scm ("property-path"), grob_property_path);
    }
  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Pop_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Push_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_unset_iterator);
