/*   
     slash-repeat-engraver.cc --  implement Chord_tremolo_engraver
  
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
class Slash_repeat_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Slash_repeat_engraver);
protected:
  Repeated_music * repeat_;

  /// moment (global time) where beam started.
  Moment start_mom_;
  Moment stop_mom_;

  /// location  within measure where beam started.
  Moment beam_start_location_;
  Moment next_moment_;
  Moment body_length_;

  Item * beat_slash_;
  Item * double_percent_;
protected:
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
};

Slash_repeat_engraver::Slash_repeat_engraver ()
{
  repeat_ =0;
  beat_slash_ = 0;
}

bool
Slash_repeat_engraver::try_music (Music * m)
{
  Repeated_music * rp = dynamic_cast<Repeated_music*> (m);
  if (rp
      && !repeat_
      && rp->get_property ("iterator-ctor")
      == Percent_repeat_iterator::constructor_proc)
    {
      body_length_ = rp->body_get_length ();
      int count =   rp->repeat_count ();
      
      Moment now = now_mom ();
      start_mom_ = now;
      stop_mom_ = start_mom_ + Moment (count) * body_length_;
      next_moment_ = start_mom_ + body_length_;

      SCM m = get_property ("measureLength");
      Moment meas_len;
      if (Moment *mp = unsmob_moment (m))
	meas_len = *mp;

      if (body_length_ < meas_len 
	  && meas_len.main_part_.mod_rat (body_length_.main_part_)
	  == Moment (Rational (0,0)))
	{
	  repeat_ = rp;
	}
      else
	return false;
      
      Global_context *global =get_global_context ();
      for (int i = 0; i < count; i++)  
	global->add_moment_to_process (next_moment_ + Moment (i) * body_length_);
  
      return true;
    }

  return false;
}

void
Slash_repeat_engraver::process_music ()
{
  if (repeat_ && now_mom () == next_moment_)
    {
      beat_slash_ = make_item ("RepeatSlash", repeat_->self_scm ());
      next_moment_ = next_moment_ + body_length_;

      get_global_context ()->add_moment_to_process (next_moment_);
    }
}


void
Slash_repeat_engraver::start_translation_timestep ()
{
  if (stop_mom_ == now_mom ())
    {
      repeat_ = 0;
    }
}

void
Slash_repeat_engraver::stop_translation_timestep ()
{
  beat_slash_ = 0;
}




ENTER_DESCRIPTION (Slash_repeat_engraver,
/* descr */       "Make beat repeats.",
/* creats*/       "RepeatSlash",
/* accepts */     "repeated-music",
/* acks  */      "",
/* reads */       "measureLength",
/* write */       "");
