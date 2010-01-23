/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "change-iterator.hh"

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "warn.hh"

void
Change_iterator::error (string reason)
{
  string to_type = ly_symbol2string (get_music ()->get_property ("change-to-type"));
  string to_id = ly_scm2string (get_music ()->get_property ("change-to-id"));

  string warn1 = _f ("cannot change `%s' to `%s'", to_type, to_id)
    + ": " + reason;

  /*
    GUHG!
  */
  string warn2= "Change_iterator::process (): "
    + get_outlet ()->context_name () + " = `"
    + get_outlet ()->id_string () + "': ";
  warning (warn2);
  get_music ()->origin ()->warning (warn1);
}

/*
  move to construct_children ?
*/
void
Change_iterator::process (Moment m)
{
  Context *current = get_outlet ();
  Context *last = 0;

  SCM to_type = get_music ()->get_property ("change-to-type");
  string to_id = ly_scm2string (get_music ()->get_property ("change-to-id"));

  /* find the type  of translator that we're changing.

  If \translator Staff = bass, then look for Staff = *
  */
  while (current && !current->is_alias (to_type))
    {
      last = current;
      current = current->get_parent_context ();
    }

  if (current && current->id_string () == to_id)
    {
      string msg;
      msg += _f ("cannot change, already in translator: %s", to_id);
    }

  if (current)
    if (last)
      {
	Context *dest = 0;
	Context *where = get_outlet ();
	while (!dest && where)
	  {
	    dest = find_context_below (where, to_type, to_id);
	    where = where->get_parent_context ();
	  }

	if (dest)
	  {
	    send_stream_event (last, "ChangeParent", get_music ()->origin (),
			       ly_symbol2scm ("context"), dest->self_scm ());
	  }
	else
	  /* FIXME: constant error message.  */
	  get_music ()->origin ()->warning (_ ("cannot find context to switch to"));
      }
    else
      {
	/* We could change the current translator's id, but that would make
	   errors hard to catch.

	   last->translator_id_string () = get_change
	   ()->change_to_id_string (); */
	error (_f ("not changing to same context type: %s", ly_symbol2string (to_type).c_str ()));
      }
  else
    /* FIXME: uncomprehensable message */
    error (_ ("none of these in my family"));

  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Change_iterator);
