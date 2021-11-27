/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2018--2021 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "axis-group-interface.hh"
#include "context.hh"
#include "grob-array.hh"
#include "international.hh"
#include "item.hh"
#include "mark-engraver.hh"
#include "stream-event.hh"
#include "text-interface.hh"
#include "warn.hh"

#include "translator.icc"

/**
   Create marks such as "D.C. al Fine" outside the system.
*/
class Jump_engraver final : public Engraver
{
  bool first_time_ = true;
  bool printed_fine_ = false;
  Item *text_ = nullptr;
  Stream_event *dal_segno_ev_ = nullptr;
  Stream_event *fine_ev_ = nullptr;

public:
  TRANSLATOR_DECLARATIONS (Jump_engraver);

protected:
  void process_music ();
  void stop_translation_timestep ();

  void listen_dal_segno (Stream_event *);
  void listen_fine (Stream_event *);
};

Jump_engraver::Jump_engraver (Context *c)
  : Engraver (c)
{
}

void
Jump_engraver::listen_dal_segno (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (dal_segno_ev_, ev);
}

void
Jump_engraver::listen_fine (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (fine_ev_, ev);
}

void
Jump_engraver::process_music ()
{
  SCM m = SCM_EOL;
  Stream_event *ev = nullptr;

  if (dal_segno_ev_)
    {
      if (fine_ev_)
        {
          // Sure, we could complicate this engraver to print text for both,
          // but the resulting score would still be unclear.  A user who really
          // wants both D.S. and Fine at the same point can add text to the
          // score with other features.
          fine_ev_->warning (_i ("Ignoring Fine simultaneous with"
                                 " D.C. or D.S."));
        }

      ev = dal_segno_ev_;
      text_ = make_item ("JumpScript", ev->self_scm ());

      // We indicate D.S. to the most recent segno mark.  This would not be
      // correct for nested segno repeats, but we don't care to support those.
      SCM body_start_markup = SCM_BOOL_F; // D.C.
      auto segno_count
        = from_scm<size_t> (get_property (this, "segnoMarkCount"), 0);
      if (segno_count > 0)
        {
          SCM proc = get_property (this, "segnoMarkFormatter");
          if (ly_is_procedure (proc))
            {
              body_start_markup = scm_call_2 (proc, to_scm (segno_count),
                                              context ()->self_scm ());
            }
        }

      SCM body_end_markup = SCM_BOOL_F;
      SCM next_markup = SCM_BOOL_F;
      auto alt_num
        = from_scm<size_t> (get_property (ev, "alternative-number"), 0);
      if (alt_num > 0)
        {
          // Assuming that the coda marks of the current group of alternatives
          // are sequential, we compute the sequence number of the first one.
          auto coda_mark_count
            = from_scm<size_t> (get_property (this, "codaMarkCount"), 0);
          coda_mark_count -= (alt_num - 1);
          SCM proc = get_property (this, "codaMarkFormatter");
          if (ly_is_procedure (proc))
            {
              body_end_markup = scm_call_2 (proc, to_scm (coda_mark_count),
                                            context ()->self_scm ());
            }

          next_markup = Mark_engraver::get_current_mark_text (context ());
          // get_current_mark_text () may return SCM_EOL like a failed property
          // lookup, but our formatter expects either markup or SCM_BOOL_F.
          if (scm_is_null (next_markup))
            next_markup = SCM_BOOL_F;
        }

      if (scm_is_false (next_markup) && printed_fine_)
        {
          // Print "al Fine" if there was a "Fine" at any prior point.  This
          // heuristic might not be correct in scores with multiple segno
          // repeats, but we don't care enough to complicate this.
          body_end_markup = get_property (this, "fineText");
        }

      SCM proc = get_property (this, "dalSegnoTextFormatter");
      if (ly_is_procedure (proc))
        {
          const auto count = from_scm (get_property (ev, "return-count"), 1L);
          m = scm_call_3 (proc,
                          context ()->self_scm (),
                          to_scm (count),
                          scm_cons2 (body_start_markup,
                                     body_end_markup,
                                     scm_cons (next_markup, SCM_EOL)));
        }
    }
  else if (fine_ev_)
    {
      ev = fine_ev_;
      text_ = make_item ("JumpScript", ev->self_scm ());
      m = get_property (this, "fineText");
    }

  if (ev)
    {
      if (Text_interface::is_markup (m))
        set_property (text_, "text", m);
      else
        ev->warning (_ ("jump text must be a markup object"));
    }
}

void
Jump_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      set_object (text_, "side-support-elements",
                  grob_list_to_grob_array (get_property (this, "stavesFound")));
      text_ = nullptr;
    }

  if (fine_ev_)
    printed_fine_ = true;

  first_time_ = false;
  dal_segno_ev_ = nullptr;
  fine_ev_ = nullptr;
}

void
Jump_engraver::boot ()
{
  ADD_LISTENER (Jump_engraver, dal_segno);
  ADD_LISTENER (Jump_engraver, fine);
}

ADD_TRANSLATOR (Jump_engraver,
                /* doc */
                R"(
This engraver creates instructions such as @emph{D.C.} and @emph{Fine}, placing
them vertically outside the set of staves given in the @code{stavesFound}
context property.

If @code{Jump_@/engraver} is added or moved to another context,
@iref{Staff_collecting_engraver} also needs to be there so that marks appear at
the intended Y@tie{}location.
                )",

                /* create */
                "JumpScript ",

                /* read */
                "dalSegnoTextFormatter "
                "fineText "
                "segnoMarkCount "
                "segnoMarkFormatter "
                "stavesFound ",

                /* write */
                ""
               );
