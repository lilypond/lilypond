/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2020 Mike Solomon <mike@mikesolomon.org>

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

#include "engraver.hh"

#include "music.hh"
#include "stream-event.hh"
#include "international.hh"
#include "item.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"
#include "system.hh"

#include "translator.icc"

using std::vector;

class Footnote_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Footnote_engraver);

  void acknowledge_grob (Grob_info) override;
  void acknowledge_end_grob (Grob_info);

  vector<Drul_array<Spanner *> > annotated_spanners_;

  void finalize () override;

  void footnotify (Grob *, SCM);
};

void
Footnote_engraver::finalize ()
{
  annotated_spanners_.clear ();
}

Footnote_engraver::Footnote_engraver (Context *c)
  : Engraver (c)
{
}

void
Footnote_engraver::footnotify (Grob *g, SCM cause)
{
  Spanner *s = dynamic_cast<Spanner *>(g);

  if (s)
    {
      Spanner *b = make_spanner ("FootnoteSpanner", cause);
      b->set_parent (s, Y_AXIS);
      b->set_parent (s, X_AXIS);
      Grob *bound = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
      b->set_bound (LEFT, bound);
      annotated_spanners_.push_back (Drul_array<Spanner *> (s, b));
    }
  else
    {
      Grob *b = make_item ("FootnoteItem", cause);
      b->set_parent (g, Y_AXIS);
      b->set_parent (g, X_AXIS);
    }
}

void
Footnote_engraver::acknowledge_grob (Grob_info info)
{
  Music *mus = unsmob<Music> (get_property (info.grob (), "footnote-music"));

  if (mus)
    {
      if (!mus->is_mus_type ("footnote-event"))
        {
          mus->programming_error (_ ("Must be footnote-event."));
          return;
        }

      footnotify (info.grob (), mus->to_event ()->unprotect ());

      // This grob has exhausted its footnote
      set_property (info.grob (), "footnote-music", SCM_EOL);

      return;
    }
}

void
Footnote_engraver::acknowledge_end_grob (Grob_info info)
{
  Spanner *s = dynamic_cast<Spanner *>(info.grob ());

  if (s)
    for (vsize i = 0; i < annotated_spanners_.size (); i++)
      {
        if (annotated_spanners_[i][LEFT] == s)
          {
            Grob *bound = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
            annotated_spanners_[i][RIGHT]->set_bound (RIGHT, bound);
            break;
          }
      }
}

void
Footnote_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Footnote_engraver, grob);
  ADD_END_ACKNOWLEDGER (Footnote_engraver, grob);
}

ADD_TRANSLATOR (Footnote_engraver,
                /* doc */
                "Create footnote texts.",

                /* create */
                "FootnoteItem "
                "FootnoteSpanner ",

                /*read*/
                "currentMusicalColumn ",

                /*write*/
                ""
               );
