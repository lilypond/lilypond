/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>


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
#include "item.hh"
#include "laissez-vibrer-engraver.hh"
#include "pointer-group-interface.hh"
#include "stream-event.hh"

#include "translator.icc"

Laissez_vibrer_engraver::Laissez_vibrer_engraver (Context *c)
  : Engraver (c)
{
  event_ = 0;
  lv_column_ = 0;
}

void
Laissez_vibrer_engraver::stop_translation_timestep ()
{
  event_ = 0;
  lv_column_ = 0;
  lv_ties_.clear ();
}

void
Laissez_vibrer_engraver::listen_laissez_vibrer (Stream_event *ev)
{
  assign_event_once (event_, ev);
}

bool
Laissez_vibrer_engraver::is_my_event_class (Stream_event *ev)
{
  return ev->in_event_class ("laissez-vibrer-event");
}

Grob *
Laissez_vibrer_engraver::make_my_tie (SCM cause)
{
  return make_item ("LaissezVibrerTie", cause);
}

Grob *
Laissez_vibrer_engraver::make_my_column (SCM cause)
{
  return make_item ("LaissezVibrerTieColumn", cause);
}

void
Laissez_vibrer_engraver::acknowledge_note_head (Grob_info inf)
{
  /* use the heard event_ for all note heads, or an individual event for just
   * a single note head (attached as an articulation inside a chord) */
  Stream_event *tie_ev = event_;
  Stream_event *note_ev = inf.event_cause ();
  if (!tie_ev && note_ev && note_ev->in_event_class ("note-event"))
    {
      SCM articulations = get_property (note_ev, "articulations");
      for (SCM s = articulations; !tie_ev && scm_is_pair (s); s = scm_cdr (s))
        {
          Stream_event *ev = unsmob<Stream_event> (scm_car (s));
          if (ev && is_my_event_class (ev))
            tie_ev = ev;
        }
    }

  if (!tie_ev)
    return;

  SCM cause = tie_ev->self_scm ();

  Grob *lv_tie = make_my_tie (cause);

  if (!lv_column_)
    lv_column_ = make_my_column (lv_tie->self_scm ());

  set_object (lv_tie, "note-head", inf.grob ()->self_scm ());

  Pointer_group_interface::add_grob (lv_column_, ly_symbol2scm ("ties"),
                                     lv_tie);

  if (is_scm<Direction> (get_property (tie_ev, "direction")))
    {
      Direction d = from_scm<Direction> (get_property (tie_ev, "direction"));
      set_property (lv_tie, "direction", to_scm (d));
    }

  lv_tie->set_y_parent (lv_column_);

  lv_ties_.push_back (lv_tie);
}

void
Laissez_vibrer_engraver::boot ()
{
  ADD_LISTENER (laissez_vibrer);
  ADD_ACKNOWLEDGER (note_head);
}

ADD_TRANSLATOR (Laissez_vibrer_engraver,
                /* doc */
                R"(
Create laissez vibrer items.
                )",

                /* create */
                R"(
LaissezVibrerTie
LaissezVibrerTieColumn
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
