/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Juergen Reuter <reuter@ipd.uka.de>,
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "note-head.hh"
#include "pitch.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

using std::vector;

/*
 * This class implements an engraver for custos symbols.
 *
 * FIXME: note heads inside of ligatures (i.e. ligature heads) are
 * sometimes not recognized by this engraver. --jr
 */
class Custos_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Custos_engraver);
  void start_translation_timestep ();
  void acknowledge_note_head (Grob_info);
  void process_acknowledged ();
  void stop_translation_timestep ();
  void finalize () override;

private:
  Item *create_custos ();
  vector<Grob *> custodes_;
  vector<Pitch> pitches_;
};

Custos_engraver::Custos_engraver (Context *c)
  : Engraver (c)
{
}

void
Custos_engraver::stop_translation_timestep ()
{
  /*
    delay typeset until we're at the next moment, so we can silence custodes at the end of the piece.
  */
  pitches_.clear ();
}

void
Custos_engraver::start_translation_timestep ()
{
  custodes_.clear ();
}

void
Custos_engraver::acknowledge_note_head (Grob_info info)
{
  Stream_event *ev = info.event_cause ();
  if (ev && ev->in_event_class ("note-event"))
    {

      /*
        ideally, we'd do custos->set_parent (Y_AXIS, notehead),
        but since the note head lives on the other system, we can't

        So we copy the position from the note head pitch.  We
        don't look at the staff-position, since we can't be sure
        whether Clef_engraver already applied a vertical shift.
      */
      pitches_.push_back (*unsmob<Pitch> (get_property (ev, "pitch")));
    }
}

void
Custos_engraver::process_acknowledged ()
{
  // Efficiency: don't create Custos where they wouldn't be visible
  // anyway.
  if (break_allowed (context ()))
    {
      for (vsize i = pitches_.size (); i--;)
        {
          Item *c = create_custos ();

          int p = pitches_[i].steps ();
          SCM c0 = get_property (this, "middleCPosition");
          if (scm_is_number (c0))
            p += from_scm<int> (c0);

          set_property (c, "staff-position", to_scm (p));
        }

      pitches_.clear ();
    }
}

Item *
Custos_engraver::create_custos ()
{
  Item *custos = make_item ("Custos", SCM_EOL);

  custodes_.push_back (custos);

  return custos;
}

void
Custos_engraver::finalize ()
{
  for (vsize i = custodes_.size (); i--;)
    custodes_[i]->suicide ();
  custodes_.clear ();
}

void
Custos_engraver::boot ()
{
  ADD_ACKNOWLEDGER (note_head);
}

ADD_TRANSLATOR (Custos_engraver,
                /* doc */
                R"(
Engrave custodes.
                )",

                /* create */
                R"(
Custos
                )",

                /* read */
                R"(
forbidBreak
forceBreak
                )",

                /* write */
                R"(

                )");
