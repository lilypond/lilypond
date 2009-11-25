/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2009 Joe Neeman <joeneeman@gmail.com>

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
#include "paper-column.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

class Page_turn_event {
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
    Interval_t<Rational> intersect = intersection (duration_, penalty.duration_);
    vector<Page_turn_event> ret;

    if (intersect.is_empty ())
      {
	ret.push_back (*this);
	return ret;
      }

    Real new_pen = max (penalty_, penalty.penalty_);

    if (duration_[LEFT] < penalty.duration_[LEFT])
      ret.push_back (Page_turn_event (duration_[LEFT], penalty.duration_[LEFT], permission_, penalty_));

    if (penalty.permission_ != SCM_EOL)
      ret.push_back (Page_turn_event (intersect[LEFT], intersect[RIGHT], permission_, new_pen));

    if (penalty.duration_[RIGHT] < duration_[RIGHT])
      ret.push_back (Page_turn_event (penalty.duration_[RIGHT], duration_[RIGHT], permission_, penalty_));

    return ret;
  }
};

class Page_turn_engraver : public Engraver
{
  Moment rest_begin_;
  Moment repeat_begin_;
  Moment note_end_;
  Rational repeat_begin_rest_length_;

  vector<Page_turn_event> forced_breaks_;
  vector<Page_turn_event> automatic_breaks_;
  vector<Page_turn_event> repeat_penalties_;

  /* the next 3 are in sync (ie. same number of elements, etc.) */
  vector<Rational> breakable_moments_;
  vector<Grob*> breakable_columns_;
  vector<bool> special_barlines_;

  SCM max_permission (SCM perm1, SCM perm2);
  Real penalty (Rational rest_len);
  Grob *breakable_column (Page_turn_event const &);

protected:
  DECLARE_TRANSLATOR_LISTENER (break);
  DECLARE_ACKNOWLEDGER (note_head);

public:
  TRANSLATOR_DECLARATIONS (Page_turn_engraver);
  void stop_translation_timestep ();
  void start_translation_timestep ();
  void finalize ();
};

Page_turn_engraver::Page_turn_engraver ()
{
  repeat_begin_ = Moment (-1);
  repeat_begin_rest_length_ = 0;
  rest_begin_ = 0;
  note_end_ = 0;
}

Grob*
Page_turn_engraver::breakable_column (Page_turn_event const &brk)
{
  vsize start = lower_bound (breakable_moments_, brk.duration_[LEFT], less<Rational> ());
  vsize end = upper_bound (breakable_moments_, brk.duration_[RIGHT], less<Rational> ());

  if (start == breakable_moments_.size ())
    return NULL;
  if (end == 0)
    return NULL;
  end--;

  for (vsize i = end + 1; i-- > start;)
    if (special_barlines_[i])
      return breakable_columns_[i];

  return breakable_columns_[end];
}

Real
Page_turn_engraver::penalty (Rational rest_len)
{
  Rational min_turn = robust_scm2moment (get_property ("minimumPageTurnLength"), Moment (1)).main_part_;

  return (rest_len < min_turn) ? infinity_f : 0;
}

void
Page_turn_engraver::acknowledge_note_head (Grob_info gi)
{
  Stream_event *cause = gi.event_cause ();

  Duration *dur_ptr = cause
    ? unsmob_duration (cause->get_property ("duration"))
    : 0;
  
  if (!dur_ptr)
    return;

  if (rest_begin_ < now_mom ())
    {
      Real pen = penalty ((now_mom () - rest_begin_).main_part_);
      if (!isinf (pen))
	  automatic_breaks_.push_back (Page_turn_event (rest_begin_.main_part_,
							now_mom ().main_part_,
							ly_symbol2scm ("allow"), 0));
    }

  if (rest_begin_ <= repeat_begin_)
    repeat_begin_rest_length_ = (now_mom () - repeat_begin_).main_part_;
  note_end_ = now_mom () + dur_ptr->get_length ();
}

IMPLEMENT_TRANSLATOR_LISTENER (Page_turn_engraver, break);
void
Page_turn_engraver::listen_break (Stream_event *ev)
{
  string name = ly_scm2string (scm_symbol_to_string (ev->get_property ("class")));

  if (name == "page-turn-event")
    {
      SCM permission = ev->get_property ("break-permission");
      Real penalty = robust_scm2double (ev->get_property ("break-penalty"), 0);
      Rational now = now_mom ().main_part_;

      forced_breaks_.push_back (Page_turn_event (now, now, permission, penalty));
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

  if (breakable_columns_.size () && !Paper_column::is_breakable (breakable_columns_.back ()))
    {
      breakable_columns_.pop_back ();
      breakable_moments_.pop_back ();
      special_barlines_.pop_back ();
    }
}

void
Page_turn_engraver::stop_translation_timestep ()
{
  Grob *pc = unsmob_grob (get_property ("currentCommandColumn"));

  if (pc)
    {
      breakable_columns_.push_back (pc);
      breakable_moments_.push_back (now_mom ().main_part_);

      SCM bar_scm = get_property ("whichBar");
      string bar = robust_scm2string (bar_scm, "");

      special_barlines_.push_back (bar != "" && bar != "|");
    }

  /* C&P from Repeat_acknowledge_engraver */
  SCM cs = get_property ("repeatCommands");
  bool start = false;
  bool end = false;

  for (; scm_is_pair (cs); cs = scm_cdr (cs))
    {
      SCM command = scm_car (cs);
      if (command == ly_symbol2scm ("start-repeat"))
	start = true;
      else if (command == ly_symbol2scm ("end-repeat"))
	end = true;
    }

  if (end && repeat_begin_.main_part_ >= Moment (0))
    {
      Rational now = now_mom ().main_part_;
      Real pen = penalty ((now_mom () - rest_begin_).main_part_ + repeat_begin_rest_length_);
      Moment *m = unsmob_moment (get_property ("minimumRepeatLengthForPageTurn"));
      if (m && *m > (now_mom () - repeat_begin_))
	pen = infinity_f;

      if (pen == infinity_f)
	repeat_penalties_.push_back (Page_turn_event (repeat_begin_.main_part_, now, SCM_EOL, -infinity_f));
      else
	repeat_penalties_.push_back (Page_turn_event (repeat_begin_.main_part_, now, ly_symbol2scm ("allow"), pen));

      repeat_begin_ = Moment (-1);
    }

  if (start)
    {
      repeat_begin_ = now_mom ();
      repeat_begin_rest_length_ = 0;
    }
  rest_begin_ = note_end_;
}

/* return the most permissive symbol (where force is the most permissive and
   forbid is the least
*/
SCM
Page_turn_engraver::max_permission (SCM perm1, SCM perm2)
{
  if (perm1 == SCM_EOL)
    return perm2;
  if (perm1 == ly_symbol2scm ("allow") && perm2 == ly_symbol2scm ("force"))
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
      for (;
	   rep_index < repeat_penalties_.size ()
	     && repeat_penalties_[rep_index].duration_[RIGHT] <= brk.duration_[LEFT];
	   rep_index++)
	;

      if (rep_index >= repeat_penalties_.size ()
	  || brk.duration_[RIGHT] <= repeat_penalties_[rep_index].duration_[LEFT])
	auto_breaks.push_back (brk);
      else
	{
	  vector<Page_turn_event> split = brk.penalize (repeat_penalties_[rep_index]);

	  /* it's possible that the last of my newly-split events overlaps the next repeat_penalty,
	     in which case we need to refilter that event */
	  if (rep_index + 1 < repeat_penalties_.size ()
	      && split.size ()
	      && split.back ().duration_[RIGHT] > repeat_penalties_[rep_index+1].duration_[LEFT])
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
	  SCM perm = max_permission (pc->get_property ("page-turn-permission"), brk.permission_);
	  Real pen = min (robust_scm2double (pc->get_property ("page-turn-penalty"), infinity_f), brk.penalty_);
	  pc->set_property ("page-turn-permission", perm);
	  pc->set_property ("page-turn-penalty", scm_from_double (pen));
	}
    }

  /* unless a manual break overrides it, allow a page turn at the end of the piece */
  breakable_columns_.back ()->set_property ("page-turn-permission", ly_symbol2scm ("allow"));

  /* apply the manual breaks */
  for (vsize i = 0; i < forced_breaks_.size (); i++)
    {
      Page_turn_event const &brk = forced_breaks_[i];
      Grob *pc = breakable_column (forced_breaks_[i]);
      if (pc)
	{
	  pc->set_property ("page-turn-permission", brk.permission_);
	  pc->set_property ("page-turn-penalty", scm_from_double (brk.penalty_));
	}
    }
}

ADD_ACKNOWLEDGER (Page_turn_engraver, note_head);

ADD_TRANSLATOR (Page_turn_engraver,
                /* doc */
                "Decide where page turns are allowed to go.",

                /* create */
                "",

                /* read */
		"minimumPageTurnLength "
		"minimumRepeatLengthForPageTurn ",

                /* write */
                ""
		);
