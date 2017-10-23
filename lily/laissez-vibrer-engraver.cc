/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>


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
#include "pointer-group-interface.hh"
#include "stream-event.hh"

#include "translator.icc"

class Laissez_vibrer_engraver : public Engraver
{
  Stream_event *event_;
  Grob *lv_column_;
  vector<Grob *> lv_ties_;

  void stop_translation_timestep ();
  void acknowledge_note_head (Grob_info);
protected:
  void listen_laissez_vibrer (Stream_event *);
public:
  TRANSLATOR_DECLARATIONS (Laissez_vibrer_engraver);
};

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
  ASSIGN_EVENT_ONCE (event_, ev);
}

void
Laissez_vibrer_engraver::acknowledge_note_head (Grob_info inf)
{
  /* use the heard event_ for all note heads, or an individual event for just
   * a single note head (attached as an articulation inside a chord) */
  Stream_event *tie_ev = event_;
  Grob *head = inf.grob ();
  Stream_event *note_ev = unsmob<Stream_event> (head->get_property ("cause"));
  if (!tie_ev && note_ev && note_ev->in_event_class ("note-event"))
    {
      SCM articulations = note_ev->get_property ("articulations");
      for (SCM s = articulations; !tie_ev && scm_is_pair (s); s = scm_cdr (s))
        {
          Stream_event *ev = unsmob<Stream_event> (scm_car (s));
          if (ev && ev->in_event_class ("laissez-vibrer-event"))
            tie_ev = ev;
        }
    }

  if (!tie_ev)
    return;

  SCM cause = tie_ev->self_scm ();

  Grob *lv_tie = make_item ("LaissezVibrerTie", cause);

  if (!lv_column_)
    lv_column_ = make_item ("LaissezVibrerTieColumn", lv_tie->self_scm ());

  lv_tie->set_object ("note-head", head->self_scm ());

  Pointer_group_interface::add_grob (lv_column_, ly_symbol2scm ("ties"),
                                     lv_tie);

  if (is_direction (unsmob<Stream_event> (cause)->get_property ("direction")))
    {
      Direction d = to_dir (unsmob<Stream_event> (cause)->get_property ("direction"));
      lv_tie->set_property ("direction", scm_from_int (d));
    }

  lv_tie->set_parent (lv_column_, Y_AXIS);

  lv_ties_.push_back (lv_tie);
}

void
Laissez_vibrer_engraver::boot ()
{
  ADD_LISTENER (Laissez_vibrer_engraver, laissez_vibrer);
  ADD_ACKNOWLEDGER (Laissez_vibrer_engraver, note_head);
}

ADD_TRANSLATOR (Laissez_vibrer_engraver,
                /* doc */
                "Create laissez vibrer items.",

                /* create */
                "LaissezVibrerTie "
                "LaissezVibrerTieColumn ",

                /* read */
                "",

                /* write */
                ""
               );
