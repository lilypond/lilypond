/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2018--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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
  bool final_fine_text_visibility_ = false;
  Item *ad_hoc_jump_text_ = nullptr;
  Item *ds_text_ = nullptr;
  Item *fine_text_ = nullptr;
  Stream_event *ad_hoc_jump_ev_ = nullptr;
  Stream_event *ds_ev_ = nullptr;
  Stream_event *fine_ev_ = nullptr;

public:
  TRANSLATOR_DECLARATIONS (Jump_engraver);

protected:
  void start_translation_timestep ();
  void process_music ();
  void stop_translation_timestep ();
  void finalize () override;

  void listen_ad_hoc_jump (Stream_event *);
  void listen_dal_segno (Stream_event *);
  void listen_fine (Stream_event *);
};

Jump_engraver::Jump_engraver (Context *c)
  : Engraver (c)
{
}

void
Jump_engraver::start_translation_timestep ()
{
  fine_text_ = nullptr;
}

void
Jump_engraver::listen_ad_hoc_jump (Stream_event *ev)
{
  assign_event_once (ad_hoc_jump_ev_, ev);
}

void
Jump_engraver::listen_dal_segno (Stream_event *ev)
{
  assign_event_once (ds_ev_, ev);
}

void
Jump_engraver::listen_fine (Stream_event *ev)
{
  assign_event_once (fine_ev_, ev);
}

void
Jump_engraver::process_music ()
{
  if (ad_hoc_jump_ev_)
    {
      ad_hoc_jump_text_
        = make_item ("JumpScript", ad_hoc_jump_ev_->self_scm ());

      SCM m = get_property (ad_hoc_jump_ev_, "text");
      if (Text_interface::is_markup (m))
        set_property (ad_hoc_jump_text_, "text", m);
      else
        ad_hoc_jump_ev_->warning (_ ("jump text must be a markup object"));
    }

  if (ds_ev_)
    {
      ds_text_ = make_item ("JumpScript", ds_ev_->self_scm ());

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
              body_start_markup
                = ly_call (proc, to_scm (segno_count), context ()->self_scm ());
            }
        }

      SCM body_end_markup = SCM_BOOL_F;
      SCM next_markup = SCM_BOOL_F;
      auto alt_num
        = from_scm<size_t> (get_property (ds_ev_, "alternative-number"), 0);
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
              body_end_markup = ly_call (proc, to_scm (coda_mark_count),
                                         context ()->self_scm ());
            }

          next_markup
            = Mark_engraver::get_current_performance_mark_text (context ());
          // Mark_engraver may return SCM_EOL like a failed property lookup,
          // but our formatter expects either markup or SCM_BOOL_F.
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

      SCM m = SCM_EOL;
      SCM proc = get_property (this, "dalSegnoTextFormatter");
      if (ly_is_procedure (proc))
        {
          const auto count
            = from_scm (get_property (ds_ev_, "return-count"), 1L);
          m = ly_call (proc, context ()->self_scm (), to_scm (count),
                       scm_cons2 (body_start_markup, body_end_markup,
                                  scm_cons (next_markup, SCM_EOL)));
        }

      if (Text_interface::is_markup (m))
        set_property (ds_text_, "text", m);
      else
        ds_ev_->warning (_ ("jump text must be a markup object"));
    }

  if (fine_ev_)
    {
      fine_text_ = make_item ("JumpScript", fine_ev_->self_scm ());

      SCM m = get_property (this, "fineText");
      if (Text_interface::is_markup (m))
        set_property (fine_text_, "text", m);
      else
        fine_ev_->warning (_ ("jump text must be a markup object"));

      // We don't know yet whether this is the last timestep, but if it is, we
      // will need to honor finalFineTextVisibility.
      final_fine_text_visibility_
        = from_scm<bool> (get_property (this, "finalFineTextVisibility"));
    }
}

void
Jump_engraver::stop_translation_timestep ()
{
  SCM staves_found = SCM_UNDEFINED;
  for (Item *const text : {ds_text_, fine_text_, ad_hoc_jump_text_})
    {
      if (text)
        {
          if (SCM_UNBNDP (staves_found))
            staves_found = get_property (this, "stavesFound");

          set_object (text, "side-support-elements",
                      grob_list_to_grob_array (staves_found));
        }
    }

  if (fine_ev_)
    printed_fine_ = true;

  ds_ev_ = nullptr;
  ds_text_ = nullptr;
  fine_ev_ = nullptr;
  first_time_ = false;
  ad_hoc_jump_ev_ = nullptr;
  ad_hoc_jump_text_ = nullptr;
}

void
Jump_engraver::finalize ()
{
  // By default, avoid printing "Fine" at the written end of the music.
  // These cases are noteworthy:
  //
  // * Repeats have been unfolded.  No other repeat notation remains, so
  //   leaving "Fine" would look strange.
  //
  // * It is more convenient to code an optionally unfoldable piece as
  //       \repeat volta 2 { ... } \fine
  //   than
  //       \repeat volta 2 { ... \volta 2 \unfolded \bar "|." }
  if (fine_text_ && !final_fine_text_visibility_)
    {
      fine_text_->suicide ();
      fine_text_ = nullptr;
    }
}

void
Jump_engraver::boot ()
{
  ADD_LISTENER (ad_hoc_jump);
  ADD_LISTENER (dal_segno);
  ADD_LISTENER (fine);
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
                R"(
JumpScript
                )",

                /* read */
                R"(
codaMarkCount
codaMarkFormatter
dalSegnoTextFormatter
finalFineTextVisibility
fineText
segnoMarkCount
segnoMarkFormatter
stavesFound
                )",

                /* write */
                R"(

                )");
