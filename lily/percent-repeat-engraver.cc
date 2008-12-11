/*
  percent-repeat-engraver.cc -- implement Percent_repeat_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>, Erik Sandberg <mandolaerik@gmail.com>
*/


#include "score-engraver.hh"

#include "bar-line.hh"
#include "global-context.hh"
#include "international.hh"
#include "item.hh"
#include "misc.hh"
#include "repeated-music.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

/*
* TODO: Create separate Double_percent_repeat_engraver? 
* Or, at least move double percent handling to Slash_repeat_engraver
*/

class Percent_repeat_engraver : public Engraver
{
  void typeset_perc ();
  bool check_count_visibility (SCM count);
public:
  TRANSLATOR_DECLARATIONS (Percent_repeat_engraver);
  
protected:
  Stream_event *percent_event_;

  /// moment (global time) where percent started.
  Moment stop_mom_;
  Moment start_mom_;

  enum Repeat_sign_type
    {
      UNKNOWN,
      MEASURE,
      DOUBLE_MEASURE,
    };
  Repeat_sign_type repeat_sign_type_;

  Spanner *percent_;
  Spanner *percent_counter_;

  
  Grob *first_command_column_;
  Moment command_moment_;
  
protected:
  virtual void finalize ();
  DECLARE_TRANSLATOR_LISTENER (percent);

  void stop_translation_timestep ();
  void start_translation_timestep ();
  void process_music ();
};

Percent_repeat_engraver::Percent_repeat_engraver ()
{
  percent_ = 0;
  percent_counter_ = 0;
  percent_event_ = 0;

  first_command_column_ = 0;
  command_moment_ = Moment (-1);
}

void
Percent_repeat_engraver::start_translation_timestep ()
{
  if (now_mom ().main_part_ != command_moment_.main_part_)
    {
      first_command_column_ = unsmob_grob (get_property ("currentCommandColumn"));
      command_moment_ = now_mom ();
    }

  if (stop_mom_.main_part_ == now_mom ().main_part_)
    {
      if (percent_)
      	typeset_perc ();
      percent_event_ = 0;
      repeat_sign_type_ = UNKNOWN;
    }
}

IMPLEMENT_TRANSLATOR_LISTENER (Percent_repeat_engraver, percent);
void
Percent_repeat_engraver::listen_percent (Stream_event *ev)
{
  if (!percent_event_)
    {
      Moment body_length = get_event_length (ev);
      Moment meas_len (robust_scm2moment (get_property ("measureLength"),
					  Moment (1)));
      if (meas_len == body_length)
	{
	  repeat_sign_type_ = MEASURE;
	  start_mom_ = now_mom ();
	  stop_mom_ = now_mom () + body_length;
	  get_global_context ()->add_moment_to_process (stop_mom_);
	}
      else if (Moment (2) * meas_len == body_length)
	{
	  repeat_sign_type_ = DOUBLE_MEASURE;
	  start_mom_ = now_mom () + meas_len;
	  stop_mom_ = now_mom () + body_length; /* never used */
	  get_global_context ()->add_moment_to_process (start_mom_);
	}
      else
	{
	  /*
	    don't warn about percent repeats: slash repeats are not
	    exactly 1 or 2 measures long.
	  */
	  return;
	}
      percent_event_ = ev;
    }
  else
    /* print a warning: no assignment happens because
       percent_event_ != 0 */
    ASSIGN_EVENT_ONCE (percent_event_, ev);
}

void
Percent_repeat_engraver::process_music ()
{
  if (percent_event_ && now_mom ().main_part_ == start_mom_.main_part_)
    {
      if (repeat_sign_type_ == MEASURE)
	{
	  if (percent_)
	    typeset_perc ();
	  
	  percent_ = make_spanner ("PercentRepeat", percent_event_->self_scm ());

	  Grob *col = first_command_column_;
	  percent_->set_bound (LEFT, col);

	  SCM count = percent_event_->get_property ("repeat-count");
	  if (count != SCM_EOL && to_boolean (get_property ("countPercentRepeats"))
	      && check_count_visibility (count))
	    {
	      percent_counter_
		= make_spanner ("PercentRepeatCounter", percent_event_->self_scm ());

	      SCM text = scm_number_to_string (count, scm_from_int (10));
	      percent_counter_->set_property ("text", text);
	      percent_counter_->set_bound (LEFT, col);
	      Side_position_interface::add_support (percent_counter_,
						    percent_);
	      percent_counter_->set_parent (percent_, Y_AXIS);
	    }
	  else
	    percent_counter_ = 0;
	}
      else if (repeat_sign_type_ == DOUBLE_MEASURE)
	{
	  Item *double_percent = make_item ("DoublePercentRepeat", percent_event_->self_scm ());

	  SCM count = percent_event_->get_property ("repeat-count");
	  if (count != SCM_EOL && to_boolean (get_property ("countPercentRepeats"))
	      && check_count_visibility (count))
	    {
	      Item *double_percent_counter = make_item ("DoublePercentRepeatCounter",
	      						percent_event_->self_scm ());

	      SCM text = scm_number_to_string (count,
					       scm_from_int (10));
	      double_percent_counter->set_property ("text", text);

	      Side_position_interface::add_support (double_percent_counter,
						    double_percent);
	      double_percent_counter->set_parent (double_percent, Y_AXIS);
	      double_percent_counter->set_parent (double_percent, X_AXIS);
	    }
	  
	  /* forbid breaks on a % line. Should forbid all breaks, really. */
	  context ()->get_score_context ()->set_property ("forbidBreak", SCM_BOOL_T);

	  /* No more processing needed. */
	  repeat_sign_type_ = UNKNOWN;
	}
    }
}

void
Percent_repeat_engraver::finalize ()
{
  if (percent_)
    {
      percent_event_->origin ()->warning (_ ("unterminated percent repeat"));
      percent_->suicide ();
      percent_counter_->suicide ();
    }
}

void
Percent_repeat_engraver::typeset_perc ()
{
  if (percent_)
    {
      Grob *col = first_command_column_;

      percent_->set_bound (RIGHT, col);
      percent_ = 0;

      if (percent_counter_)
	percent_counter_->set_bound (RIGHT, col);
      percent_counter_ = 0;
    }
}

bool
Percent_repeat_engraver::check_count_visibility (SCM count)
{
  SCM proc = get_property ("repeatCountVisibility");
  return (ly_is_procedure (proc) && to_boolean (scm_call_2 (proc,
							    count,
							    context ()->self_scm ())));
}


void
Percent_repeat_engraver::stop_translation_timestep ()
{
}

ADD_TRANSLATOR (Percent_repeat_engraver,
		/* doc */
		"Make whole bar and double bar repeats.",
		
		/* create */
		"DoublePercentRepeat "
		"DoublePercentRepeatCounter "
		"PercentRepeat "
		"PercentRepeatCounter ",

		/* read */
		"countPercentRepeats "
		"currentCommandColumn "
		"measureLength "
		"repeatCountVisibility ",

		/* write */
		"forbidBreak "
		);
