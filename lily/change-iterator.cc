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

#include "change-iterator.hh"

#include "context.hh"
#include "input.hh"
#include "international.hh"
#include "music.hh"
#include "warn.hh"

using std::string;

void
Change_iterator::error (const string &reason)
{
  string to_type = ly_symbol2string (get_property (get_music (), "change-to-type"));
  string to_id = ly_scm2string (get_property (get_music (), "change-to-id"));

  string warn1 = _f ("cannot change `%s' to `%s'", to_type, to_id)
                 + ": " + reason;

  /*
    GUHG!
  */
  string warn2 = "Change_iterator::process (): "
                 + get_outlet ()->context_name () + " = `"
                 + get_outlet ()->id_string () + "': ";
  ::warning (warn2);
  warning (warn1);
}

string
Change_iterator::change_to (Music_iterator &it,
                            SCM to_type,
                            const string &to_id)
{
  string result; // error message

  // Find the context that should have its parent changed.
  Context *last = find_context_above_by_parent_type (it.get_outlet (), to_type);
  if (last)
    {
      // Find the new parent.
      Context *dest = find_context_near (it.get_outlet (), to_type, to_id);
      if (dest)
        {
          send_stream_event (last, "ChangeParent", it.origin (),
                             ly_symbol2scm ("context"), dest->self_scm ());
        }
      else
        {
          it.warning (_f ("cannot find context to change to: %s",
                          Context::diagnostic_id (to_type, to_id).c_str ()));
        }
    }
  else if (it.get_outlet ()->is_alias (to_type))
    {
      // No enclosing context of the right kind was found
      // and the iterator's immediate context is the kind that was sought.

      result = _f ("not changing to same context type: %s", ly_symbol2string (to_type).c_str ());
    }
  else
    /* FIXME: incomprehensible message */
    result = _ ("none of these in my family");

  return result;
}

/*
  move to construct_children ?
*/
void
Change_iterator::process (Moment m)
{
  SCM to_type = get_property (get_music (), "change-to-type");
  string to_id = ly_scm2string (get_property (get_music (), "change-to-id"));

  string msg = change_to (*this, to_type, to_id);
  if (!msg.empty ())
    error (msg);

  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Change_iterator);
