/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
Property_iterator::process (Moment mom)
{
  Context *o = get_outlet ();
  Music *m = get_music ();

  send_stream_event (o, "SetProperty", m->origin (), ly_symbol2scm ("symbol"),
                     m->get_property ("symbol"), ly_symbol2scm ("value"),
                     m->get_property ("value"), ly_symbol2scm ("once"),
                     m->get_property ("once"));

  Simple_music_iterator::process (mom);
}

void
Property_unset_iterator::process (Moment mom)
{
  Context *o = get_outlet ();
  Music *m = get_music ();

  send_stream_event (o, "UnsetProperty", m->origin (), ly_symbol2scm ("symbol"),
                     m->get_property ("symbol"), ly_symbol2scm ("once"),
                     m->get_property ("once"));

  Simple_music_iterator::process (mom);
}

bool
check_grob (Music *mus, SCM sym)
{
  bool g = to_boolean (scm_object_property (sym, ly_symbol2scm ("is-grob?")));

  if (!g)
    mus->origin ()->warning (
        _f ("not a grob name, `%s'", ly_symbol2string (sym)));

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
      SCM once = get_music ()->get_property ("once");

      if (to_boolean (get_music ()->get_property ("pop-first"))
          && !to_boolean (once))
        send_stream_event (get_outlet (), "Revert", get_music ()->origin (),
                           ly_symbol2scm ("symbol"), sym,
                           ly_symbol2scm ("property-path"), grob_property_path);

      send_stream_event (get_outlet (), "Override", get_music ()->origin (),
                         ly_symbol2scm ("symbol"), sym,
                         ly_symbol2scm ("property-path"), grob_property_path,
                         ly_symbol2scm ("once"), once, ly_symbol2scm ("value"),
                         val);
    }
  Simple_music_iterator::process (m);
}

void
Pop_property_iterator::process (Moment mom)
{
  Music *m = get_music ();
  SCM sym = m->get_property ("symbol");

  if (check_grob (m, sym))
    {
      SCM grob_property_path = get_property_path (m);

      send_stream_event (get_outlet (), "Revert", m->origin (),
                         ly_symbol2scm ("symbol"), sym, ly_symbol2scm ("once"),
                         m->get_property ("once"),
                         ly_symbol2scm ("property-path"), grob_property_path);
    }
  Simple_music_iterator::process (mom);
}

IMPLEMENT_CTOR_CALLBACK (Pop_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Push_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_unset_iterator);
