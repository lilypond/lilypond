/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "context.hh"
#include "duration.hh"
#include "grob-array.hh"
#include "item.hh"
#include "music.hh"
#include "stream-event.hh"
#include "text-interface.hh"

#include "translator.icc"

#include <cctype>

class Metronome_mark_engraver : public Engraver
{
  Item *text_;
  Grob *support_;
  Grob *bar_;
  Stream_event *tempo_ev_;

public:
  TRANSLATOR_DECLARATIONS (Metronome_mark_engraver);

protected:
  void stop_translation_timestep ();
  void process_music ();

  void acknowledge_break_aligned (Grob_info_t<Item>);
  void acknowledge_break_alignment (Grob_info_t<Item>);
  void acknowledge_grob (Grob_info) override;

  void listen_tempo_change (Stream_event *);
};

Metronome_mark_engraver::Metronome_mark_engraver (Context *c)
  : Engraver (c)
{
  text_ = 0;
  support_ = 0;
  bar_ = 0;
  tempo_ev_ = 0;
}

void
Metronome_mark_engraver::listen_tempo_change (Stream_event *ev)
{
  assign_event_once (tempo_ev_, ev);
}

static bool
safe_is_member (SCM scm, SCM lst)
{
  return ly_is_list (lst) && scm_is_true (scm_member (scm, lst));
}

void
Metronome_mark_engraver::acknowledge_break_aligned (Grob_info_t<Item> info)
{
  Grob *g = info.grob ();

  if (text_
      && scm_is_eq (get_property (g, "break-align-symbol"),
                    ly_symbol2scm ("staff-bar")))
    bar_ = g;
  else if (text_ && !support_
           && safe_is_member (get_property (g, "break-align-symbol"),
                              get_property (text_, "break-align-symbols")))
    {
      support_ = g;
      text_->set_x_parent (g);
    }
  if (bar_ || support_)
    set_property (text_, "non-musical", SCM_BOOL_T);
}

void
Metronome_mark_engraver::acknowledge_break_alignment (Grob_info_t<Item> info)
{
  if (text_ && support_)
    text_->set_x_parent (info.grob ());
}

void
Metronome_mark_engraver::acknowledge_grob (Grob_info info)
{
  Grob *g = info.grob ();

  if (text_)
    for (SCM s = get_property (text_, "non-break-align-symbols");
         scm_is_pair (s); s = scm_cdr (s))
      if (g->internal_has_interface (scm_car (s)))
        text_->set_x_parent (g);
}

void
Metronome_mark_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      if (text_->get_x_parent ()
          && text_->get_x_parent ()->internal_has_interface (
            ly_symbol2scm ("multi-measure-rest-interface"))
          && bar_)
        text_->set_x_parent (bar_);
      else if (!support_)
        {
          /*
            Gardner Read "Music Notation", p.278

            Align the metronome mark over the time signature (or the
            first notational element of the measure if no time
            signature is present in that measure).
          */
          if (Grob *mc
              = unsmob<Grob> (get_property (this, "currentMusicalColumn")))
            text_->set_x_parent (mc);
          else if (Grob *cc
                   = unsmob<Grob> (get_property (this, "currentCommandColumn")))
            text_->set_x_parent (cc);
        }
      set_object (text_, "side-support-elements",
                  grob_list_to_grob_array (get_property (this, "stavesFound")));
      text_ = 0;
      support_ = 0;
      bar_ = 0;
      tempo_ev_ = 0;
    }
}

void
Metronome_mark_engraver::process_music ()
{
  if (tempo_ev_)
    {
      text_ = make_item ("MetronomeMark", tempo_ev_->self_scm ());

      SCM proc = get_property (this, "metronomeMarkFormatter");
      SCM result
        = ly_call (proc, tempo_ev_->self_scm (), context ()->self_scm ());

      set_property (text_, "text", result);
    }
}

void
Metronome_mark_engraver::boot ()
{
  ADD_LISTENER (tempo_change);
  ADD_ACKNOWLEDGER (break_aligned);
  ADD_ACKNOWLEDGER (break_alignment);
  ADD_ACKNOWLEDGER (grob);
}

ADD_TRANSLATOR (Metronome_mark_engraver,
                /* doc */
                R"(
Engrave metronome marking.  This delegates the formatting work to the function
in the @code{metronomeMarkFormatter} property.  The mark is put over all
staves.  The staves are taken from the @code{stavesFound} property, which is
maintained by @ref{Staff_collecting_engraver}.
                )",

                /* create */
                R"(
MetronomeMark
                )",

                /* read */
                R"(
currentCommandColumn
currentMusicalColumn
metronomeMarkFormatter
stavesFound
tempoHideNote
                )",

                /* write */
                R"(

                )");
