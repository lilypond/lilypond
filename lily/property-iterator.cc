/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  Context *o = get_context ();
  Music *m = get_music ();

  send_stream_event (o, "SetProperty", m->origin (), ly_symbol2scm ("symbol"),
                     get_property (m, "symbol"), ly_symbol2scm ("value"),
                     get_property (m, "value"), ly_symbol2scm ("once"),
                     get_property (m, "once"));

  Simple_music_iterator::process (mom);
}

void
Property_unset_iterator::process (Moment mom)
{
  Context *o = get_context ();
  Music *m = get_music ();

  send_stream_event (o, "UnsetProperty", m->origin (), ly_symbol2scm ("symbol"),
                     get_property (m, "symbol"), ly_symbol2scm ("once"),
                     get_property (m, "once"));

  Simple_music_iterator::process (mom);
}

bool
check_grob (Music *mus, SCM sym)
{
  bool g
    = from_scm<bool> (scm_object_property (sym, ly_symbol2scm ("is-grob?")));

  if (!g)
    mus->warning (_f ("not a grob name, `%s'", ly_symbol2string (sym)));

  return g;
}

SCM
get_property_path (Music *m)
{
  SCM grob_property_path = get_property (m, "grob-property-path");

  SCM eprop = get_property (m, "grob-property");
  if (scm_is_symbol (eprop))
    {
      grob_property_path = ly_list (eprop);
    }

  return grob_property_path;
}

void
Push_property_iterator::process (Moment m)
{
  SCM sym = get_property (get_music (), "symbol");
  if (check_grob (get_music (), sym))
    {
      SCM grob_property_path = get_property_path (get_music ());
      SCM val = get_property (get_music (), "grob-value");
      SCM once = get_property (get_music (), "once");

      if (from_scm<bool> (get_property (get_music (), "pop-first"))
          && !from_scm<bool> (once))
        send_stream_event (get_context (), "Revert", origin (),
                           ly_symbol2scm ("symbol"), sym,
                           ly_symbol2scm ("property-path"), grob_property_path);

      send_stream_event (
        get_context (), "Override", origin (), ly_symbol2scm ("symbol"), sym,
        ly_symbol2scm ("property-path"), grob_property_path,
        ly_symbol2scm ("once"), once, ly_symbol2scm ("value"), val);
    }
  Simple_music_iterator::process (m);
}

void
Pop_property_iterator::process (Moment mom)
{
  Music *m = get_music ();
  SCM sym = get_property (m, "symbol");

  if (check_grob (m, sym))
    {
      SCM grob_property_path = get_property_path (m);

      send_stream_event (get_context (), "Revert", m->origin (),
                         ly_symbol2scm ("symbol"), sym, ly_symbol2scm ("once"),
                         get_property (m, "once"),
                         ly_symbol2scm ("property-path"), grob_property_path);
    }
  Simple_music_iterator::process (mom);
}

IMPLEMENT_CTOR_CALLBACK (Pop_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Push_property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_iterator);
IMPLEMENT_CTOR_CALLBACK (Property_unset_iterator);
