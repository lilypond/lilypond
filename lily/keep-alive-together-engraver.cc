/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2011 Joe Neeman <joeneeman@gmail.com>

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

#include "context.hh"
#include "dispatcher.hh"
#include "engraver.hh"
#include "grob.hh"
#include "grob-array.hh"

#include "translator.icc"

class Keep_alive_together_engraver: public Engraver
{
  vector<Grob *> group_spanners_;

public:
  TRANSLATOR_DECLARATIONS (Keep_alive_together_engraver);
  DECLARE_ACKNOWLEDGER (hara_kiri_group_spanner);

  virtual void finalize ();
};

Keep_alive_together_engraver::Keep_alive_together_engraver ()
{
}

void
Keep_alive_together_engraver::acknowledge_hara_kiri_group_spanner (Grob_info i)
{
  group_spanners_.push_back (i.grob ());
}

void
Keep_alive_together_engraver::finalize ()
{
  for (vsize i = 0; i < group_spanners_.size (); ++i)
    {
      SCM grob_array_scm = Grob_array::make_array ();
      Grob_array *ga = unsmob_grob_array (grob_array_scm);

      // It would make Hara_kiri_group_spanner::request_suicide a _little_
      // faster if we removed each grob from its own array. It seems
      // unnecessary for now, though.
      ga->set_array (group_spanners_);
      group_spanners_[i]->set_object ("keep-alive-with", grob_array_scm);
    }
}

ADD_ACKNOWLEDGER (Keep_alive_together_engraver, hara_kiri_group_spanner);

ADD_TRANSLATOR (Keep_alive_together_engraver,
                /* doc */
                "This engraver collects all @code{Hara_kiri_group_spanner}s "
                "that are created in contexts at or below its own.  "
                "These spanners are then tied together so that one will "
                "be removed only if all are removed.  For example, "
                "if a @code{StaffGroup} uses this engraver, then the staves "
                "in the group will all be visible as long as there is a note "
                "in at least one of them.",

                /* create */
                "",

                /* read */
                "",

                /* write */
                ""
               );
