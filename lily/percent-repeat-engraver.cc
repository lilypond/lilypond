/*   
  new-chord-tremolo-engraver.cc --  implement Chord_tremolo_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "repeated-music.hh"
#include "engraver-group-engraver.hh"
#include "global-translator.hh"
#include "warn.hh"
#include "misc.hh"
#include "spanner.hh"
#include "item.hh"
#include "percent-repeat-iterator.hh"
#include "bar.hh"

#include "score-engraver.hh"
#include "translator-group.hh"

/**
  This acknowledges repeated music with "percent" style.  It typesets
  a % sign.  

  TODO:

  - BEAT case: Create items for single beat repeats, i.e. c4 / / /

  - DOUBLE_MEASURE case: attach a % to an appropriate barline.
  
*/
class Percent_repeat_engraver : public Engraver
{
  void typeset_perc ();
public:
  TRANSLATOR_DECLARATIONS(Percent_repeat_engraver);
protected:
  Repeated_music * repeat_;

  /// moment (global time) where beam started.
  Moment start_mom_;
  Moment stop_mom_;

  /// location  within measure where beam started.
  Moment beam_start_location_;
  Moment next_moment_;
  Moment body_length_;

  enum {
    UNKNOWN,
    BEAT,
    MEASURE,
    DOUBLE_MEASURE,
  } repeat_sign_type_ ;

  Item * beat_slash_;
  Item * double_percent_;
  Spanner * perc_p_;
  Spanner * finished_perc_p_;
  Item * stem_tremolo_;
protected:
  virtual void finalize ();
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
};

Percent_repeat_engraver::Percent_repeat_engraver ()
{
  perc_p_  = finished_perc_p_ = 0;
  repeat_ =0;
  stem_tremolo_ = 0;

  beat_slash_ = 0;
  double_percent_ = 0;
}

bool
Percent_repeat_engraver::try_music (Music * m)
{
  Repeated_music * rp = dynamic_cast<Repeated_music*> (m);
  if (rp
      && rp->get_mus_property ("iterator-ctor")
         == Percent_repeat_iterator::constructor_cxx_function
      && !repeat_)
    {
      body_length_ = rp->body_length_mom ();
      int count =   rp->repeat_count ();
      
      Moment now = now_mom ();
      start_mom_ = now;
      stop_mom_ = start_mom_ + Moment (count) * body_length_;
      next_moment_ = start_mom_ + body_length_;

      SCM m = get_property ("measureLength");
      Moment meas_len;
      if (unsmob_moment (m))
	meas_len = *unsmob_moment (m);

      if (body_length_ < meas_len &&
	  meas_len.main_part_.mod_rat (body_length_.main_part_) == Moment (Rational (0,0)))
	repeat_sign_type_ = BEAT;
      else if (meas_len == body_length_)
	repeat_sign_type_ = MEASURE;
      else if (Moment (2)* meas_len == body_length_)
	{
	  repeat_sign_type_ = DOUBLE_MEASURE;
	  next_moment_ += meas_len ;
	}
      else
	{
	  warning (_ ("Don't know how to handle a percent repeat of this length."));
	  return false;
	}

      repeat_ = rp;

      
      Global_translator *global_l =0;
      Translator *t = this;
      do
	{
	  t = t->daddy_trans_l_ ;
	  global_l = dynamic_cast<Global_translator*> (t);
	}
      while (!global_l);

      for (int i = 0; i < count; i++)  
	global_l->add_moment_to_process (now + Moment (1+i) * body_length_);
  
      return true;
    }

  return false;
}

void
Percent_repeat_engraver::process_music ()
{
  if (repeat_ && now_mom () == next_moment_)
    {
      if (repeat_sign_type_ == BEAT)
	{
	  beat_slash_ = new Item (get_property ("RepeatSlash"));
	  announce_grob(beat_slash_, repeat_->self_scm());
	}
      else if (repeat_sign_type_ == MEASURE)
	{
	  finished_perc_p_ = perc_p_;
	  typeset_perc ();
	  perc_p_ = new Spanner (get_property ("PercentRepeat"));
	  SCM col =get_property ("currentCommandColumn");
	  perc_p_->set_bound (LEFT, unsmob_grob (col));
	  announce_grob(perc_p_, repeat_->self_scm());
	}
      else if (repeat_sign_type_ == DOUBLE_MEASURE)
	
	{
	  double_percent_ = new Item (get_property ("DoublePercentRepeat"));
	  announce_grob(double_percent_, repeat_->self_scm());

      /*
	forbid breaks on a % line. Should forbid all breaks, really.
       */

	    top_engraver()->forbid_breaks ();	// guh. Use properties!      
	}
      next_moment_ = next_moment_ + body_length_;
    }
}

void
Percent_repeat_engraver::finalize ()
{
  typeset_perc ();
  if (perc_p_)
    {
      repeat_->origin ()->warning (_ ("unterminated chord tremolo"));
      perc_p_->suicide ();
    }
}

void
Percent_repeat_engraver::typeset_perc ()
{
  if (finished_perc_p_)
    {
      SCM col =get_property ("currentCommandColumn");
      finished_perc_p_->set_bound (RIGHT, unsmob_grob (col));
      typeset_grob (finished_perc_p_);
      finished_perc_p_ = 0;
    }

  if (beat_slash_)
    {
      typeset_grob (beat_slash_);
      beat_slash_ = 0;
    }

  if (double_percent_)
    {
      typeset_grob (double_percent_);
      double_percent_ = 0;
    }
}




void
Percent_repeat_engraver::start_translation_timestep ()
{
  if (stop_mom_ == now_mom ())
    {
      if (perc_p_)
	{
	  finished_perc_p_ = perc_p_;
	  typeset_perc ();
	}
      repeat_ = 0;
      perc_p_ = 0;
      repeat_sign_type_ = UNKNOWN;
    }
}


void
Percent_repeat_engraver::stop_translation_timestep ()
{
  typeset_perc ();
}




ENTER_DESCRIPTION(Percent_repeat_engraver,
/* descr */       "Make beat, whole bar and double bar repeats.",
/* creats*/       "PercentRepeat RepeatSlash DoublePercentRepeat",
/* acks  */       "",
/* reads */       "measureLength currentCommandColumn",
/* write */       "");
