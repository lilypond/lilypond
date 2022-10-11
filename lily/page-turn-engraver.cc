/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Joe Neeman <joeneeman@gmail.com>

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
#include "grob.hh"
#include "international.hh"
#include "ly-scm-list.hh"
#include "paper-column.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

#include <algorithm>
#include <vector>

using std::string;
using std::vector;

class Page_turn_event
{
public:
  SCM permission_;
  Real penalty_;
  Interval_t<Rational> duration_;

  Page_turn_event (Rational start, Rational end, SCM perm, Real pen)
  {
    duration_[LEFT] = start;
    duration_[RIGHT] = end;
    permission_ = perm;
    penalty_ = pen;
  }

  /* Suppose we have decided on a possible page turn, only to change
     out mind later (for example, if there is a volta repeat and it
     would be difficult to turn the page back). Then we need to
     re-penalize a region of the piece. Depending on how the events
     intersect, we may have to split it into as many as 3 pieces.
  */
  vector<Page_turn_event> penalize (Page_turn_event const &penalty)
  {
    Interval_t<Rational> intersect
      = intersection (duration_, penalty.duration_);
    vector<Page_turn_event> ret;

    if (intersect.is_empty ())
      {
        ret.push_back (*this);
        return ret;
      }

    Real new_pen = std::max (penalty_, penalty.penalty_);

    if (duration_[LEFT] < penalty.duration_[LEFT])
      ret.push_back (Page_turn_event (duration_[LEFT], penalty.duration_[LEFT],
                                      permission_, penalty_));

    if (!scm_is_null (penalty.permission_))
      ret.push_back (Page_turn_event (intersect[LEFT], intersect[RIGHT],
                                      permission_, new_pen));

    if (penalty.duration_[RIGHT] < duration_[RIGHT])
      ret.push_back (Page_turn_event (penalty.duration_[RIGHT],
                                      duration_[RIGHT], permission_, penalty_));

    return ret;
  }
};

class Page_turn_engraver final : public Engraver
{
  Moment rest_begin_ {0};
  Moment repeat_begin_ {-1};
  Moment note_end_ {0};
  Rational repeat_begin_rest_length_ {0};
  bool found_special_bar_line_ {false};

  vector<Page_turn_event> forced_breaks_;
  vector<Page_turn_event> automatic_breaks_;
  vector<Page_turn_event> repeat_penalties_;

  /* the next 3 are in sync (ie. same number of elements, etc.) */
  vector<Rational> breakable_moments_;
  vector<Grob *> breakable_columns_;
  vector<bool> special_barlines_;

  SCM max_permission (SCM perm1, SCM perm2);
  Real penalty (Rational rest_len);
  Grob *breakable_column (Page_turn_event const &);

protected:
  void listen_break (Stream_event *);
  void acknowledge_bar_line (Grob_info_t<Item>);
  void acknowledge_note_head (Grob_info);

public:
  TRANSLATOR_DECLARATIONS (Page_turn_engraver);
  void stop_translation_timestep ();
  void start_translation_timestep ();
  void finalize () override;
};

Page_turn_engraver::Page_turn_engraver (Context *c)
  : Engraver (c)
{
}

Grob *
Page_turn_engraver::breakable_column (Page_turn_event const &brk)
{
  auto start
    = std::lower_bound (breakable_moments_.begin (), breakable_moments_.end (),
                        brk.duration_[LEFT]);
  auto end = std::upper_bound (breakable_moments_.begin (),
                               breakable_moments_.end (), brk.duration_[RIGHT]);

  if (start == breakable_moments_.end ())
    return NULL;
  if (end == breakable_moments_.begin ())
    return NULL;
  // Use (signed) long to avoid problems in the loop below when start_idx = 0.
  long start_idx = start - breakable_moments_.begin ();
  long end_idx = end - breakable_moments_.begin () - 1;

  for (long i = end_idx; i >= start_idx; i--)
    if (special_barlines_[i])
      return breakable_columns_[i];

  return breakable_columns_[end_idx];
}

Real
Page_turn_engraver::penalty (Rational rest_len)
{
  const auto min_turn
    = from_scm (get_property (this, "minimumPageTurnLength"), Moment (1))
        .main_part_;

  return (rest_len < min_turn) ? infinity_f : 0;
}

static bool
is_bar_line_special (const string &glyph)
{
  // TODO: This is crude.  In a number of cases (but not when the bar line
  // glyph was forced with the \bar command), there is at least one knowable
  // cause for a bar line, e.g., a \section command.  Moreover, an event that
  // might signal a good turning point doesn't necessarily cause a special bar
  // line, e.g., a segno mark aligned on a measure boundary, or an ancient
  // finalis sign that is rendered with BreathingSign.  Page_turn_engraver
  // could listen to those events in addition to watching for bar lines.
  return (glyph != "") && (glyph != "|");
}

void
Page_turn_engraver::acknowledge_bar_line (Grob_info_t<Item> gi)
{
  if (!found_special_bar_line_)
    {
      auto glyph = robust_scm2string (get_property (gi.grob (), "glyph"), "");
      found_special_bar_line_ = is_bar_line_special (glyph);
    }
}

void
Page_turn_engraver::acknowledge_note_head (Grob_info gi)
{
  Stream_event *cause = gi.event_cause ();

  Duration *dur_ptr
    = cause ? unsmob<Duration> (get_property (cause, "duration")) : 0;

  if (!dur_ptr)
    return;

  if (rest_begin_ < now_mom ())
    {
      Real pen = penalty ((now_mom () - rest_begin_).main_part_);
      if (!std::isinf (pen))
        automatic_breaks_.push_back (
          Page_turn_event (rest_begin_.main_part_, now_mom ().main_part_,
                           ly_symbol2scm ("allow"), 0));
    }

  if (rest_begin_ <= repeat_begin_)
    repeat_begin_rest_length_ = (now_mom () - repeat_begin_).main_part_;
  note_end_ = now_mom () + dur_ptr->get_length ();
}

void
Page_turn_engraver::listen_break (Stream_event *ev)
{
  string name = ly_symbol2string (scm_car (get_property (ev, "class")));

  if (name == "page-turn-event")
    {
      SCM permission = get_property (ev, "break-permission");
      Real penalty = from_scm<double> (get_property (ev, "break-penalty"), 0);
      Rational now = now_mom ().main_part_;

      forced_breaks_.push_back (
        Page_turn_event (now, now, permission, penalty));
    }
}

void
Page_turn_engraver::start_translation_timestep ()
{
  /* What we want to do is to build a list of all the
     breakable paper columns. In general, paper-columns won't be marked as
     such until the Paper_column_engraver has done stop_translation_timestep.

     Therefore, we just grab /all/ paper columns (in the
     stop_translation_timestep, since they're not created here yet)
     and remove the non-breakable ones at the beginning of the following
     timestep.
  */

  if (breakable_columns_.size ()
      && !Paper_column::is_breakable (breakable_columns_.back ()))
    {
      breakable_columns_.pop_back ();
      breakable_moments_.pop_back ();
      special_barlines_.pop_back ();
    }
}

void
Page_turn_engraver::stop_translation_timestep ()
{
  Grob *pc = unsmob<Grob> (get_property (this, "currentCommandColumn"));

  if (pc)
    {
      // In a context below where bar lines are engraved (e.g. Voice), no bar
      // lines will be acknowledged, but if one was created above, we will find
      // it in currentBarLine.
      if (!found_special_bar_line_)
        {
          if (auto *bar = unsmob<Item> (get_property (this, "currentBarLine")))
            {
              auto glyph = robust_scm2string (get_property (bar, "glyph"), "");
              found_special_bar_line_ = is_bar_line_special (glyph);
            }
        }

      breakable_columns_.push_back (pc);
      breakable_moments_.push_back (now_mom ().main_part_);
      special_barlines_.push_back (found_special_bar_line_);
    }

  /* C&P from Repeat_acknowledge_engraver */
  bool start = false;
  bool end = false;

  SCM repeat_commands = get_property (this, "repeatCommands");
  for (SCM command : as_ly_scm_list (repeat_commands))
    {
      if (scm_is_pair (command)) // (command option...)
        command = scm_car (command);

      if (scm_is_eq (command, ly_symbol2scm ("start-repeat")))
        start = true;
      else if (scm_is_eq (command, ly_symbol2scm ("end-repeat")))
        end = true;
    }

  if (end && (repeat_begin_.main_part_ >= 0))
    {
      Rational now = now_mom ().main_part_;
      Real pen = penalty ((now_mom () - rest_begin_).main_part_
                          + repeat_begin_rest_length_);

      const auto minimumRepeatLengthForPageTurn
        = from_scm (get_property (this, "minimumRepeatLengthForPageTurn"),
                    -Moment::infinity ());
      if (minimumRepeatLengthForPageTurn > (now_mom () - repeat_begin_))
        pen = infinity_f;

      if (pen == infinity_f)
        repeat_penalties_.push_back (Page_turn_event (
          repeat_begin_.main_part_, now, SCM_EOL, -infinity_f));
      else
        repeat_penalties_.push_back (Page_turn_event (
          repeat_begin_.main_part_, now, ly_symbol2scm ("allow"), pen));

      repeat_begin_ = Moment (-1);
    }

  if (start)
    {
      repeat_begin_ = now_mom ();
      repeat_begin_rest_length_ = 0;
    }
  rest_begin_ = note_end_;

  found_special_bar_line_ = false;
}

/* return the most permissive symbol (where force is the most permissive and
   forbid is the least
*/
SCM
Page_turn_engraver::max_permission (SCM perm1, SCM perm2)
{
  if (scm_is_null (perm1))
    return perm2;
  if (scm_is_eq (perm1, ly_symbol2scm ("allow"))
      && scm_is_eq (perm2, ly_symbol2scm ("force")))
    return perm2;
  return perm1;
}

void
Page_turn_engraver::finalize ()
{
  vsize rep_index = 0;
  vector<Page_turn_event> auto_breaks;

  /* filter the automatic breaks through the repeat penalties */
  for (vsize i = 0; i < automatic_breaks_.size (); i++)
    {
      Page_turn_event &brk = automatic_breaks_[i];

      /* find the next applicable repeat penalty */
      for (; rep_index < repeat_penalties_.size ()
             && repeat_penalties_[rep_index].duration_[RIGHT]
                  <= brk.duration_[LEFT];
           rep_index++)
        ;

      if (rep_index >= repeat_penalties_.size ()
          || brk.duration_[RIGHT]
               <= repeat_penalties_[rep_index].duration_[LEFT])
        auto_breaks.push_back (brk);
      else
        {
          vector<Page_turn_event> split
            = brk.penalize (repeat_penalties_[rep_index]);

          /* it's possible that the last of my newly-split events overlaps the next repeat_penalty,
             in which case we need to refilter that event */
          if (rep_index + 1 < repeat_penalties_.size () && split.size ()
              && split.back ().duration_[RIGHT]
                   > repeat_penalties_[rep_index + 1].duration_[LEFT])
            {
              automatic_breaks_[i] = split.back ();
              split.pop_back ();
              i--;
            }
          auto_breaks.insert (auto_breaks.end (), split.begin (), split.end ());
        }
    }

  /* apply the automatic breaks */
  for (vsize i = 0; i < auto_breaks.size (); i++)
    {
      Page_turn_event const &brk = auto_breaks[i];
      Grob *pc = breakable_column (auto_breaks[i]);
      if (pc)
        {
          SCM perm = max_permission (get_property (pc, "page-turn-permission"),
                                     brk.permission_);
          Real pen
            = std::min (from_scm<double> (
                          get_property (pc, "page-turn-penalty"), infinity_f),
                        brk.penalty_);
          set_property (pc, "page-turn-permission", perm);
          set_property (pc, "page-turn-penalty", to_scm (pen));
        }
    }

  /* unless a manual break overrides it, allow a page turn at the end of the piece */
  set_property (breakable_columns_.back (), "page-turn-permission",
                ly_symbol2scm ("allow"));

  /* apply the manual breaks */
  for (vsize i = 0; i < forced_breaks_.size (); i++)
    {
      Page_turn_event const &brk = forced_breaks_[i];
      Grob *pc = breakable_column (forced_breaks_[i]);
      if (pc)
        {
          set_property (pc, "page-turn-permission", brk.permission_);
          set_property (pc, "page-turn-penalty", to_scm (brk.penalty_));
        }
    }
}

void
Page_turn_engraver::boot ()
{
  ADD_LISTENER (break);
  ADD_ACKNOWLEDGER (bar_line);
  ADD_ACKNOWLEDGER (note_head);
}

ADD_TRANSLATOR (Page_turn_engraver,
                /* doc */
                R"(
Decide where page turns are allowed to go.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
currentBarLine
minimumPageTurnLength
minimumRepeatLengthForPageTurn
                )",

                /* write */
                R"(

                )");
