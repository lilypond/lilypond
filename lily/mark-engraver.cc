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

#include "mark-engraver.hh"

#include "axis-group-interface.hh"
#include "context.hh"
#include "grob-array.hh"
#include "international.hh"
#include "item.hh"
#include "mark-tracking-translator.hh"
#include "stream-event.hh"
#include "text-interface.hh"
#include "warn.hh"

#include "translator.icc"

#include <cctype>

Mark_engraver::Mark_engraver (Context *c)
  : Engraver (c)
{
}

void
Mark_engraver::start_translation_timestep ()
{
  performance_mark_state_ = {};
  rehearsal_mark_state_ = {};
}

void
Mark_engraver::stop_translation_timestep ()
{
  auto process_mark = [this] (Mark_state &mark) {
    if (mark.text_)
      {
        if (first_time_)
          {
            // A mark created at the very beginning is always visible even if
            // it would not be visible at the beginning of a broken line.
            set_property (mark.text_, "break-visibility",
                          scm_c_make_vector (3, SCM_BOOL_T));
          }

        set_object (
          mark.text_, "side-support-elements",
          grob_list_to_grob_array (get_property (this, "stavesFound")));
        mark.final_text_ = mark.text_;
        mark.text_ = nullptr;
      }
  };

  process_mark (performance_mark_state_);
  process_mark (rehearsal_mark_state_);
  first_time_ = false;
}

void
Mark_engraver::finalize ()
{
  auto finalize_mark = [this] (Mark_state &mark) {
    if (mark.final_text_)
      {
        // A mark created at the very end is always visible even if it would
        // not be visible at the end of a broken line.
        set_property (mark.final_text_, "break-visibility",
                      scm_c_make_vector (3, SCM_BOOL_T));
      }
    mark.final_text_ = nullptr;
  };

  finalize_mark (performance_mark_state_);
  finalize_mark (rehearsal_mark_state_);
}

SCM
Mark_engraver::get_current_performance_mark_text (Context *context)
{
  const char *grob_name = nullptr;
  SCM text = SCM_EOL;
  get_current_performance_mark (context, &grob_name, &text);
  return text;
}

SCM
Mark_engraver::get_current_performance_mark (Context *ctx,
                                             const char **grob_name, SCM *text)
{
  *grob_name = nullptr;
  *text = SCM_EOL;

  // Get the event chosen by Mark_tracking_translator.
  SCM ev_scm = get_property (ctx, "currentPerformanceMarkEvent");
  auto *const ev = unsmob<Stream_event> (ev_scm);
  if (!ev)
    return SCM_EOL;

  if (ev->in_event_class ("coda-mark-event"))
    {
      *grob_name = "CodaMark";

      const auto label
        = Mark_tracking_translator::get_coda_mark_label (ctx, ev);
      if (label > 0)
        {
          SCM proc = get_property (ctx, "codaMarkFormatter");
          if (ly_is_procedure (proc))
            *text = ly_call (proc, to_scm (label), ctx->self_scm ());
        }
    }
  else if (ev->in_event_class ("section-label-event"))
    {
      *grob_name = "SectionLabel";

      *text = get_property (ev, "text");
    }
  else if (ev->in_event_class ("segno-mark-event"))
    {
      *grob_name = "SegnoMark";

      const auto label
        = Mark_tracking_translator::get_segno_mark_label (ctx, ev);
      if (label > 0)
        {
          SCM proc = get_property (ctx, "segnoMarkFormatter");
          if (ly_is_procedure (proc))
            *text = ly_call (proc, to_scm (label), ctx->self_scm ());
        }
    }

  return ev_scm;
}

SCM
Mark_engraver::get_current_rehearsal_mark_text (Context *context)
{
  const char *grob_name = nullptr;
  SCM text = SCM_EOL;
  get_current_rehearsal_mark (context, &grob_name, &text);
  return text;
}

SCM
Mark_engraver::get_current_rehearsal_mark (Context *ctx, const char **grob_name,
                                           SCM *text)
{
  *grob_name = nullptr;
  *text = SCM_EOL;

  // Get the event chosen by Mark_tracking_translator.
  SCM ev_scm = get_property (ctx, "currentRehearsalMarkEvent");
  auto *const ev = unsmob<Stream_event> (ev_scm);
  if (!ev)
    return SCM_EOL;

  if (ev->in_event_class ("rehearsal-mark-event"))
    {
      *grob_name = "RehearsalMark";

      const auto label
        = Mark_tracking_translator::get_rehearsal_mark_label (ctx, ev);
      if (label > 0)
        {
          SCM proc = get_property (ctx, "rehearsalMarkFormatter");
          if (ly_is_procedure (proc))
            *text = ly_call (proc, to_scm (label), ctx->self_scm ());
        }
    }
  else // ad-hoc-mark-event
    {
      *grob_name = "RehearsalMark";

      *text = get_property (ev, "text");
    }

  return ev_scm;
}

void
Mark_engraver::process_music ()
{
  auto process_mark = [this] (Mark_state &mark, auto get_current_mark) {
    if (!mark.text_)
      {
        const char *grob_name = nullptr;
        SCM text = SCM_EOL;
        SCM ev_scm = get_current_mark (context (), &grob_name, &text);
        if (auto *const ev = unsmob<Stream_event> (ev_scm))
          {
            mark.text_ = make_item (grob_name, ev->self_scm ());
            if (Text_interface::is_markup (text))
              set_property (mark.text_, "text", text);
            else
              ev->warning (_ ("mark label must be a markup object"));
          }
      }
  };

  process_mark (performance_mark_state_,
                Mark_engraver::get_current_performance_mark);

  process_mark (rehearsal_mark_state_,
                Mark_engraver::get_current_rehearsal_mark);
}

void
Mark_engraver::boot ()
{
}

ADD_TRANSLATOR (Mark_engraver,
                /* doc */
                R"(
This engraver creates rehearsal marks, segno and coda marks, and section
labels.

@code{Mark_@/engraver} creates marks, formats them, and places them vertically
outside the set of staves given in the @code{stavesFound} context property.

If @code{Mark_@/engraver} is added or moved to another context,
@iref{Staff_collecting_engraver} also needs to be there so that marks appear at
the intended Y@tie{}location.

By default, @code{Mark_@/engravers} in multiple contexts create a common
sequence of marks chosen by the @code{Score}-level
@iref{Mark_tracking_translator}.  If independent sequences are desired,
multiple @code{Mark_tracking_translators} must be used.
                )",

                /* create */
                R"(
CodaMark
RehearsalMark
SectionLabel
SegnoMark
                )",

                /* read */
                R"(
codaMarkFormatter
currentPerformanceMarkEvent
currentRehearsalMarkEvent
rehearsalMarkFormatter
segnoMarkFormatter
stavesFound
                )",

                /* write */
                R"(

                )");
