/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2014 Joe Neeman <joeneeman@gmail.com>

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
      SCM this_layer = group_spanners_[i]->get_property ("remove-layer");
      if (scm_is_false (this_layer))
        continue;

      SCM live_scm = Grob_array::make_array ();
      Grob_array *live = Grob_array::unsmob (live_scm);
      SCM dead_scm = Grob_array::make_array ();
      Grob_array *dead = Grob_array::unsmob (dead_scm);

      for (vsize j = 0; j < group_spanners_.size (); ++j)
        {
          if (i == j)
            continue;
          SCM that_layer = group_spanners_[j]->get_property ("remove-layer");
          if (scm_is_false (that_layer))
            continue;
          if (!scm_is_integer (this_layer))
            {
              // Unspecified layers are kept alive by anything else
              live->add (group_spanners_[j]);
              continue;
            }
          // an explicit layer is only affected by explicit layers
          if (!scm_is_integer (that_layer))
            continue;
          if (scm_is_true (scm_num_eq_p (that_layer, this_layer)))
            live->add (group_spanners_[j]);
          else if (scm_is_true (scm_less_p (that_layer, this_layer)))
            dead->add (group_spanners_[j]);
        }
      if (!live->empty ())
        group_spanners_[i]->set_object ("keep-alive-with", live_scm);
      if (!dead->empty ())
        group_spanners_[i]->set_object ("make-dead-when", dead_scm);
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
