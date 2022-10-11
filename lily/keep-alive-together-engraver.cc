/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2022 Joe Neeman <joeneeman@gmail.com>

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
#include "engraver.hh"
#include "grob.hh"
#include "grob-array.hh"
#include "international.hh"

#include "translator.icc"

using std::vector;

class Keep_alive_together_engraver : public Engraver
{
  vector<Grob *> group_spanners_;

public:
  TRANSLATOR_DECLARATIONS (Keep_alive_together_engraver);
  void acknowledge_hara_kiri_group_spanner (Grob_info);

  void finalize () override;
};

Keep_alive_together_engraver::Keep_alive_together_engraver (Context *c)
  : Engraver (c)
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
      SCM this_layer = get_property (group_spanners_[i], "remove-layer");
      if (scm_is_false (this_layer))
        continue;

      SCM live_scm = Grob_array::make_array ();
      Grob_array *live = unsmob<Grob_array> (live_scm);
      SCM dead_scm = Grob_array::make_array ();
      Grob_array *dead = unsmob<Grob_array> (dead_scm);

      for (vsize j = 0; j < group_spanners_.size (); ++j)
        {
          if (i == j)
            continue;

          if (scm_is_symbol (this_layer))
            {
              if (scm_is_eq (this_layer, ly_symbol2scm ("any")))
                {
                  // layer is kept alive by any other layer
                  live->add (group_spanners_[j]);
                  continue;
                }
              else if (scm_is_eq (this_layer, ly_symbol2scm ("above")))
                {
                  // layer is kept alive by the layer preceding it
                  if (i == j + 1)
                    live->add (group_spanners_[j]);
                  continue;
                }
              else if (scm_is_eq (this_layer, ly_symbol2scm ("below")))
                {
                  // layer is kept alive by the layer following it
                  if (i == j - 1)
                    live->add (group_spanners_[j]);
                  continue;
                }
              else
                {
                  group_spanners_[i]->warning (
                    _f ("unknown remove-layer value `%s'",
                        ly_symbol2string (this_layer).c_str ()));
                  continue;
                }
            }

          SCM that_layer = get_property (group_spanners_[j], "remove-layer");

          if (scm_is_false (that_layer))
            continue;
          if (!scm_is_integer (this_layer))
            {
              // unset layers are kept alive by all but ignored layers
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
        set_object (group_spanners_[i], "keep-alive-with", live_scm);
      if (!dead->empty ())
        set_object (group_spanners_[i], "make-dead-when", dead_scm);
    }
}

void
Keep_alive_together_engraver::boot ()
{
  ADD_ACKNOWLEDGER (hara_kiri_group_spanner);
}

ADD_TRANSLATOR (Keep_alive_together_engraver,
                /* doc */
                R"(
This engraver collects all @code{Hara_kiri_group_spanner}s that are created in
contexts at or below its own.  These spanners are then tied together so that
one will be removed only if all are removed.  For example, if a
@code{StaffGroup} uses this engraver, then the staves in the group will all be
visible as long as there is a note in at least one of them.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
