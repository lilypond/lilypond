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

#include "duration.hh"
#include "engraver.hh"
#include "international.hh"
#include "item.hh"
#include "moment.hh"
#include "note-column.hh"
#include "paper-column.hh"
#include "rest.hh"
#include "rest-collision.hh"
#include "rhythmic-head.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

#include <set>

using std::set;

class Rest_collision_engraver : public Engraver
{
protected:
  Grob *rest_collision_;

  void process_acknowledged ();
  void stop_translation_timestep ();

public:
  TRANSLATOR_DECLARATIONS (Rest_collision_engraver);
};

Rest_collision_engraver::Rest_collision_engraver (Context *c)
  : Engraver (c)
{
  rest_collision_ = 0;
}

void
Rest_collision_engraver::process_acknowledged ()
{
  vsize rest_count = 0;
  set<Grob *> columns;
  auto now = now_mom ();

  for (SCM s = get_property (this, "busyGrobs"); scm_is_pair (s);
       s = scm_cdr (s))
    {
      Grob *g = unsmob<Grob> (scm_cdar (s));
      Moment *m = unsmob<Moment> (scm_caar (s));
      if (!g || !m)
        continue;

      if (has_interface<Rhythmic_head> (g) && (*m) > now)
        {
          auto *const column = dynamic_cast<Item *> (g->get_x_parent ());
          if (!column)
            continue;

          // Only include rests that start now. Include notes that started any time.
          auto *const paper_column = column->get_column ();
          if (!has_interface<Rest> (g) || !paper_column
              || Paper_column::when_mom (paper_column) == now)
            {
              columns.insert (column);
              rest_count += Note_column::has_rests (column);
            }
        }
    }

  if (!rest_collision_ && rest_count && columns.size () > 1)
    {
      rest_collision_ = make_item ("RestCollision", SCM_EOL);
      for (set<Grob *>::iterator i = columns.begin (); i != columns.end (); ++i)
        Rest_collision::add_column (rest_collision_, *i);
    }
}

void
Rest_collision_engraver::stop_translation_timestep ()
{
  rest_collision_ = 0;
}

void
Rest_collision_engraver::boot ()
{
}

ADD_TRANSLATOR (Rest_collision_engraver,
                /* doc */
                R"(
Handle collisions of rests.
                )",

                /* create */
                R"(
RestCollision
                )",

                /* read */
                R"(
busyGrobs
                )",

                /* write */
                R"(

                )");
