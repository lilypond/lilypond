/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2015--2020 Daniel Eble <dan@faithful.be>

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

#include "change-sequence-iterator.hh"
#include "context.hh"
#include "music.hh"

using std::string;

void
Change_sequence_iterator::create_children ()
{
  Music_wrapper_iterator::create_children ();

  change_list_ = get_property (get_music (), "context-change-list");
}

void
Change_sequence_iterator::process (Moment m)
{
  // Find the ID of the output context to use now.  The loop is a bit of
  // paranoia; we shouldn't expect multiple changes between moments in this
  // part.
  SCM context_id = SCM_EOL;
  for (; scm_is_pair (change_list_); change_list_ = scm_cdr (change_list_))
    {
      SCM mom_scm = scm_caar (change_list_);
      Moment *mom = unsmob<Moment> (mom_scm);
      if (mom)
        {
          if (*mom > m)
            break;

          context_id = scm_cdar (change_list_);
        }
      else
        {
          string s = "expected moment in change list: ";
          s += ly_scm2string (mom_scm);
          programming_error (s);
        }
    }

  if (!scm_is_null (context_id))
    change_to (ly_symbol2string (context_id));

  Music_wrapper_iterator::process (m);
}
