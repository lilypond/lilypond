/*   
  new-chord-tremolo-engraver.cc --  implement Chord_tremolo_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "repeated-music.hh"
#include "engraver-group-engraver.hh"
#include "global-context.hh"
#include "warn.hh"
#include "misc.hh"
#include "spanner.hh"
#include "item.hh"
#include "percent-repeat-iterator.hh"
#include "bar-line.hh"

#include "score-engraver.hh"
#include "context.hh"


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
  TRANSLATOR_DECLARATIONS (Percent_repeat_engraver);
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
    MEASURE,
    DOUBLE_MEASURE,
  } repeat_sign_type_ ;

  Item * double_percent_;
  Spanner * perc_;
  Spanner * finished_perc_;
protected:
  virtual void finalize ();
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
};

Percent_repeat_engraver::Percent_repeat_engraver ()
{
  perc_  = 0;
  finished_perc_ = 0;
  repeat_ =0;

  double_percent_ = 0;
}

bool
Percent_repeat_engraver::try_music (Music * m)
{
  Repeated_music * rp = dynamic_cast<Repeated_music*> (m);
  if (rp
      && rp->get_property ("iterator-ctor")
         == Percent_repeat_iterator::constructor_proc
      && !repeat_)
    {
      body_length_ = rp->body_get_length ();
      int count =   rp->repeat_count ();
      
      Moment now = now_mom ();
      start_mom_ = now;
      stop_mom_ = start_mom_ + Moment (count) * body_length_;
      next_moment_ = start_mom_ + body_length_;

      SCM m = get_property ("measureLength");
      Moment meas_len;
      if (unsmob_moment (m))
	meas_len = *unsmob_moment (m);

      if (meas_len == body_length_)
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

      
      Global_context *global = get_global_context ();
      for (int i = 0; i < count; i++)
	{
	  global->add_moment_to_process (next_moment_ + Moment (i) * body_length_);
  
	  /*
	    bars between % too.
	  */
	  if (repeat_sign_type_ == DOUBLE_MEASURE)
	    global->add_moment_to_process (next_moment_ + meas_len + Moment (i) * body_length_);
	}
      return true;
    }

  return false;
}

void
Percent_repeat_engraver::process_music ()
{
  if (repeat_ && now_mom () == next_moment_)
    {
      if (repeat_sign_type_ == MEASURE)
	{
	  finished_perc_ = perc_;
	  typeset_perc ();
	  perc_ = make_spanner ("PercentRepeat");
	  SCM col =get_property ("currentCommandColumn");
	  perc_->set_bound (LEFT, unsmob_grob (col));
	  announce_grob (perc_, repeat_->self_scm ());
	}
      else if (repeat_sign_type_ == DOUBLE_MEASURE)
	{
	  double_percent_ = make_item ("DoublePercentRepeat");
	  announce_grob (double_percent_, repeat_->self_scm ());

      /*
	forbid breaks on a % line. Should forbid all breaks, really.
       */

	  get_score_engraver ()->forbid_breaks ();	// guh. Use properties!      
	}
      next_moment_ = next_moment_ + body_length_;

      get_global_context ()->add_moment_to_process (next_moment_);
    }
}

void
Percent_repeat_engraver::finalize ()
{
  typeset_perc ();
  if (perc_)
    {
      repeat_->origin ()->warning (_ ("unterminated percent repeat"));
      perc_->suicide ();
    }
}

void
Percent_repeat_engraver::typeset_perc ()
{
  if (finished_perc_)
    {
      SCM col =get_property ("currentCommandColumn");
      finished_perc_->set_bound (RIGHT, unsmob_grob (col));
      typeset_grob (finished_perc_);
      finished_perc_ = 0;
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
      if (perc_)
	{
	  finished_perc_ = perc_;
	  typeset_perc ();
	}
      repeat_ = 0;
      perc_ = 0;
      repeat_sign_type_ = UNKNOWN;
    }
}


void
Percent_repeat_engraver::stop_translation_timestep ()
{
  typeset_perc ();
}




ENTER_DESCRIPTION (Percent_repeat_engraver,
/* descr */       "Make whole bar and double bar repeats.",
/* creats*/       "PercentRepeat DoublePercentRepeat",
/* accepts */     "repeated-music",
/* acks  */      "",
/* reads */       "measureLength currentCommandColumn",
/* write */       "");
