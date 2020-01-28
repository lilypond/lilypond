/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2020 Carl D. Sorensen

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

#include <cctype>
#include <cstdio>

#include "engraver.hh"

#include "context.hh"
#include "item.hh"
#include "spanner.hh"

#include "translator.icc"

using std::vector;

/*
   Change tab-note-head properties when a tie is followed by a
   slurs or glissando.
*/
class Tab_tie_follow_engraver : public Engraver
{
  vector<Spanner *> slurs_;
  vector<Spanner *> glissandi_;
  vector<Item *> note_heads_;

public:
  TRANSLATOR_DECLARATIONS (Tab_tie_follow_engraver);

protected:
  void acknowledge_glissando (Grob_info);
  void acknowledge_slur (Grob_info);
  void acknowledge_tab_note_head (Grob_info);

  void stop_translation_timestep ();
};

Tab_tie_follow_engraver::Tab_tie_follow_engraver (Context *c) : Engraver (c) {}

void
Tab_tie_follow_engraver::acknowledge_glissando (Grob_info info)
{
  glissandi_.push_back (dynamic_cast<Spanner *> (info.grob ()));
}

void
Tab_tie_follow_engraver::acknowledge_tab_note_head (Grob_info info)
{
  note_heads_.push_back (dynamic_cast<Item *> (info.grob ()));
}

void
Tab_tie_follow_engraver::acknowledge_slur (Grob_info info)
{
  slurs_.push_back (dynamic_cast<Spanner *> (info.grob ()));
}

void
Tab_tie_follow_engraver::stop_translation_timestep ()
{
  for (vsize k = 0; k < note_heads_.size (); k++)
    {
      bool spanner_start = false;
      for (vsize j = 0; j < slurs_.size (); j++)
        {
          Item *left_item = slurs_[j]->get_bound (LEFT);
          if (left_item)
            {
              SCM left_cause = left_item->get_property ("cause");
              Item *slur_cause = unsmob<Item> (left_cause);
              if (slur_cause == note_heads_[k])
                {
                  note_heads_[k]->set_property ("span-start", SCM_BOOL_T);
                  spanner_start = true;
                  break;
                }
            }
        }
      if (!spanner_start)
        for (vsize j = 0; j < glissandi_.size (); j++)
          {
            Item *left_bound = glissandi_[j]->get_bound (LEFT);
            if (left_bound == note_heads_[k])
              {
                note_heads_[k]->set_property ("span-start", SCM_BOOL_T);
                break;
              }
          }
    }
  slurs_.clear ();
  glissandi_.clear ();
  note_heads_.clear ();
}

void
Tab_tie_follow_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Tab_tie_follow_engraver, slur);
  ADD_ACKNOWLEDGER (Tab_tie_follow_engraver, glissando);
  ADD_ACKNOWLEDGER (Tab_tie_follow_engraver, tab_note_head);
}

ADD_TRANSLATOR (Tab_tie_follow_engraver,
                /* doc */
                "Adjust TabNoteHead properties when a tie is followed"
                " by a slur or glissando.",

                /* create */
                " ",

                /* read */
                " ",

                /* write */
                " ");
