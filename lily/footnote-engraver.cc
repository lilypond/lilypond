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

#include <map>
#include <utility>

class Footnote_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Footnote_engraver);

  void acknowledge_grob (Grob_info) override;
  void acknowledge_end_grob (Grob_info);
  void process_acknowledged ();

  // Map annotated grob to associated footnote spanner
  std::map<Grob *, Spanner *> annotated_spanners_;
  std::vector<Spanner *> finished_spanners_;

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
  Grob *footnote = nullptr;
  if (dynamic_cast<Spanner *> (g))
    {
      Spanner *b = make_spanner ("FootnoteSpanner", cause);
      Grob *bound = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
      b->set_bound (LEFT, bound);
      annotated_spanners_.insert (std::make_pair (g, b));
      footnote = b;
    }
  else
    {
      footnote = make_item ("FootnoteItem", cause);
    }
  footnote->set_y_parent (g);
  footnote->set_x_parent (g);
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
  auto it = annotated_spanners_.find (info.grob ());
  if (it == annotated_spanners_.end ())
    return;

  finished_spanners_.push_back (it->second);
}

void
Footnote_engraver::process_acknowledged ()
{
  for (Spanner *sp : finished_spanners_)
    {
      for (LEFT_and_RIGHT (d))
        {
          Spanner *parent = dynamic_cast<Spanner *> (sp->get_parent (X_AXIS));
          Item *bound = parent->get_bound (d);
          if (!bound)
            {
              bound
                = unsmob<Item> (get_property (this, "currentMusicalColumn"));
            }
          sp->set_bound (d, bound);
        }
      announce_end_grob (sp, SCM_EOL);
    }
  finished_spanners_.clear ();
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
