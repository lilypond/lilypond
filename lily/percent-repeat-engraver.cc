/*
  new-chord-tremolo-engraver.cc -- implement Chord_tremolo_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/


#include "score-engraver.hh"

#include "bar-line.hh"
#include "global-context.hh"
#include "international.hh"
#include "item.hh"
#include "misc.hh"
#include "percent-repeat-iterator.hh"
#include "repeated-music.hh"
#include "side-position-interface.hh"
#include "spanner.hh"
#include "warn.hh"

#include "translator.icc"

class Percent_repeat_engraver : public Engraver
{
  void typeset_perc ();
public:
  TRANSLATOR_DECLARATIONS (Percent_repeat_engraver);
  
protected:
  Music *repeat_;

  /// moment (global time) where beam started.
  Moment start_mom_;
  Moment stop_mom_;

  /// location within measure where beam started.
  Moment beam_start_location_;
  Moment next_moment_;
  Moment body_length_;

  enum Repeat_sign_type
    {
      UNKNOWN,
      MEASURE,
      DOUBLE_MEASURE,
    };
  Repeat_sign_type repeat_sign_type_;

  Item *double_percent_;
  Item *double_percent_counter_;
  
  Spanner *percent_;
  Spanner *percent_counter_;
  Spanner *finished_percent_;
  Spanner *finished_percent_counter_;

  int count_;
  int total_count_; 
protected:
  virtual void finalize ();
  virtual bool try_music (Music *);

  void stop_translation_timestep ();
  void start_translation_timestep ();
  void process_music ();
};

Percent_repeat_engraver::Percent_repeat_engraver ()
{
  percent_ = 0;
  percent_counter_ = 0;

  finished_percent_ = 0;
  finished_percent_counter_ = 0;

  double_percent_ = 0;
  double_percent_counter_ = 0;

  repeat_ = 0;
  count_ = 0;
  total_count_ = 0;
}

bool
Percent_repeat_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("repeated-music")
      && m->get_property ("iterator-ctor")
      == Percent_repeat_iterator::constructor_proc
      && !repeat_)
    {
      body_length_ = Repeated_music::body_get_length (m);
      total_count_ = Repeated_music::repeat_count (m);
      
      Moment now = now_mom ();
      start_mom_ = now;
      stop_mom_ = start_mom_ + Moment (total_count_) * body_length_;
      next_moment_ = start_mom_;
      next_moment_ += body_length_;

      Moment meas_len (robust_scm2moment (get_property ("measureLength"),
					  Moment (1)));
      
      if (meas_len == body_length_)
	repeat_sign_type_ = MEASURE;
      else if (Moment (2) * meas_len == body_length_)
	repeat_sign_type_ = DOUBLE_MEASURE;
      else
	return false;
    

      repeat_ = m;

      Global_context *global = get_global_context ();
      for (int i = 1; i < total_count_; i++)
	{
	  Moment m = next_moment_ + Moment (i) * body_length_;
	  global->add_moment_to_process (m);

	  /* bars between % too.  */
	  if (repeat_sign_type_ == DOUBLE_MEASURE)
	    global->add_moment_to_process (m - meas_len);
	}

      if (repeat_sign_type_ == DOUBLE_MEASURE)
	next_moment_ += meas_len;

      count_ = 1;
      return true;
    }

  return false;
}

void
Percent_repeat_engraver::process_music ()
{
  if (repeat_ && now_mom ().main_part_ == next_moment_.main_part_)
    {
      count_ ++; 
      if (repeat_sign_type_ == MEASURE)
	{
	  finished_percent_ = percent_;
	  finished_percent_counter_ = percent_counter_;
	  
	  typeset_perc ();
	  percent_ = make_spanner ("PercentRepeat", repeat_->self_scm ());

	  Grob *col = unsmob_grob (get_property ("currentCommandColumn"));
	  percent_->set_bound (LEFT, col);

	  if (total_count_ > 2
	      && to_boolean (get_property ("countPercentRepeats")))
	    {
	      percent_counter_
		= make_spanner ("PercentRepeatCounter", repeat_->self_scm ());

	      SCM text = scm_number_to_string (scm_from_int (count_),
					       scm_from_int (10));
	      percent_counter_->set_property ("text", text);
	      percent_counter_->set_bound (LEFT, col);
	      Side_position_interface::add_support (percent_counter_,
						    percent_);
	      percent_counter_->set_parent (percent_, Y_AXIS);
	    }	  
	}
      else if (repeat_sign_type_ == DOUBLE_MEASURE)
	{
	  double_percent_ = make_item ("DoublePercentRepeat", repeat_->self_scm ());

	  if (total_count_ > 2
	      && to_boolean (get_property ("countPercentRepeats")))
	    {
	      double_percent_counter_
		= make_item ("DoublePercentRepeatCounter",
			     repeat_->self_scm());

	      SCM text = scm_number_to_string (scm_from_int (count_),
					       scm_from_int (10));
	      double_percent_counter_->set_property ("text", text);

	      Side_position_interface::add_support (double_percent_counter_,
						    double_percent_);
	      double_percent_counter_->set_parent (double_percent_, Y_AXIS);
	      double_percent_counter_->set_parent (double_percent_, X_AXIS);
	    }
	  
	  /*
	    forbid breaks on a % line. Should forbid all breaks, really.

	    Ugh. Why can't this be regular communication between
	    contexts?
	  */
	  get_score_engraver ()->forbid_breaks ();
	}
      next_moment_ = next_moment_ + body_length_;
      next_moment_.grace_part_ = Rational (0);
    }
}

void
Percent_repeat_engraver::finalize ()
{
  typeset_perc ();
  if (percent_)
    {
      repeat_->origin ()->warning (_ ("unterminated percent repeat"));
      percent_->suicide ();
      percent_counter_->suicide();
    }
}

void
Percent_repeat_engraver::typeset_perc ()
{
  if (finished_percent_)
    {
      Grob *col = unsmob_grob (get_property ("currentCommandColumn"));

      finished_percent_->set_bound (RIGHT, col);
      finished_percent_ = 0;

      if (finished_percent_counter_)
	finished_percent_counter_->set_bound (RIGHT, col);
    
      finished_percent_counter_ = 0;
    }

  double_percent_ = 0;
  double_percent_counter_ = 0;
}

void
Percent_repeat_engraver::start_translation_timestep ()
{
  if (stop_mom_.main_part_ == now_mom ().main_part_)
    {
      if (percent_)
	{
	  finished_percent_ = percent_;
	  finished_percent_counter_ = percent_counter_;

	  typeset_perc ();
	}
      repeat_ = 0;
      percent_ = 0;
      
      percent_counter_ = 0;
      repeat_sign_type_ = UNKNOWN;
    }
}

void
Percent_repeat_engraver::stop_translation_timestep ()
{
  typeset_perc ();
}

ADD_TRANSLATOR (Percent_repeat_engraver,
		/* doc */
		"Make whole bar and double bar repeats.",
		
		/* create */
		"PercentRepeat "
		"DoublePercentRepeat "
		"PercentRepeatCounter "
		"DoublePercentRepeatCounter",
		
		/* accept */
		"repeated-music",

		/* read */
		"measureLength "
		"currentCommandColumn "
		"countPercentRepeats",

		/* write */ "");
