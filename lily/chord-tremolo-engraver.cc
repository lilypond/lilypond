/*   
  new-chord-tremolo-engraver.cc --  implement Chord_tremolo_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "beam.hh"
#include "repeated-music.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "engraver-group-engraver.hh"
#include "musical-request.hh"
#include "warn.hh"
#include "misc.hh"
#include "note-head.hh"
#include "spanner.hh"
#include "item.hh"
#include "chord-tremolo-iterator.hh"
#include "stem-tremolo.hh"
#include "music-list.hh"
#include "math.h"           // ceil

/**
  This acknowledges repeated music with "tremolo" style.  It typesets
  a beam.

  TODO:

  - perhaps use engraver this to steer other engravers? That would
  create dependencies between engravers, which is bad.

  - create dots if appropriate.

  - create  TremoloBeam iso Beam?
 */

class Chord_tremolo_engraver : public Engraver
{
  void typeset_beam ();
TRANSLATOR_DECLARATIONS(Chord_tremolo_engraver);
protected:
  Repeated_music * repeat_;

  /// moment (global time) where beam started.
  Moment start_mom_;
  Moment stop_mom_;
  int flags_ ;
  /// location  within measure where beam started.
  Moment beam_start_location_;

  bool sequential_body_b_;
  Spanner * beam_;
  Spanner * finished_beam_;
  Item * stem_tremolo_;
protected:
  virtual void finalize ();
  virtual bool try_music (Music*);
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
};

Chord_tremolo_engraver::Chord_tremolo_engraver ()
{
  beam_  = finished_beam_ = 0;
  repeat_ =0;
  flags_ = 0;
  stem_tremolo_ = 0;
  sequential_body_b_ = false;
}

bool
Chord_tremolo_engraver::try_music (Music * m)
{
  Repeated_music * rp = dynamic_cast<Repeated_music*> (m);
  if (rp
      && rp->get_mus_property ("iterator-ctor") == Chord_tremolo_iterator::constructor_proc
      && !repeat_) 
    {
      Moment l = rp->length_mom ();
      repeat_ = rp;
      start_mom_ = now_mom ();
      stop_mom_ = start_mom_ + l;
      sequential_body_b_ = dynamic_cast<Sequential_music*> (rp->body ());

      Rational total_dur = l.main_part_;
      Rational note_dur = (total_dur / Rational (repeat_->repeat_count ()));
       flags_ = intlog2 ((total_dur / note_dur).num ());
      
      return true;
    }

  return false;
}

void
Chord_tremolo_engraver::process_music ()
{
  if (repeat_)
    {
      if (sequential_body_b_ && !beam_)
	{
	  beam_ = new Spanner (get_property ("Beam"));
	  beam_->set_grob_property ("chord-tremolo", SCM_BOOL_T);

	  SCM smp = get_property ("measurePosition");
	  Moment mp
	    = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
	  beam_start_location_ = mp;
	  announce_grob(beam_, repeat_->self_scm());
	}
      else if (!sequential_body_b_ && !stem_tremolo_)
	{

	  if (flags_)
	    {
	      stem_tremolo_ = new Item (get_property ("StemTremolo"));
	      announce_grob(stem_tremolo_, repeat_->self_scm());
	      stem_tremolo_->set_grob_property ("flag-count",
						scm_int2num (flags_));

	    }
	}
    }
}
void
Chord_tremolo_engraver::finalize ()
{
  typeset_beam ();
  if (beam_)
    {
      repeat_->origin ()->warning (_ ("unterminated chord tremolo"));
      beam_->suicide ();
    }
}

void
Chord_tremolo_engraver::typeset_beam ()
{
  if (finished_beam_)
    {
      typeset_grob (finished_beam_);
      finished_beam_ = 0;
    }
}


void
Chord_tremolo_engraver::acknowledge_grob (Grob_info info)
{
  if (beam_ && Stem::has_interface (info.grob_))
    {
      Grob * s = info.grob_;
      Stem::set_beaming (s, flags_, LEFT);
      Stem::set_beaming (s, flags_, RIGHT);
	  
      SCM d = s->get_grob_property ("direction");
      if (Stem::duration_log (s) != 1)
	{
	  beam_->set_grob_property ("gap", gh_double2scm (0.8));
	}
      s->set_grob_property ("direction", d);

      if (dynamic_cast <Rhythmic_req *> (info.music_cause ()))
	{
	  Beam::add_stem (beam_, s);
	}
      else
	{
	  String s = _ ("stem must have Rhythmic structure");
	  if (info.music_cause ())
	    info.music_cause ()->origin ()->warning (s);
	  else
	    ::warning (s);
	}
    }
  else if (stem_tremolo_ && Stem::has_interface (info.grob_))
    {
       Stem_tremolo::set_stem (stem_tremolo_, info.grob_);
       stem_tremolo_->set_parent (info.grob_,X_AXIS);
    }
}


void
Chord_tremolo_engraver::start_translation_timestep ()
{
  if (beam_ && stop_mom_ == now_mom ())
    {
      finished_beam_ = beam_;

      repeat_ = 0;
      beam_ = 0;
    }
}


void
Chord_tremolo_engraver::stop_translation_timestep ()
{
  typeset_beam ();

  if (stem_tremolo_)
    {
      typeset_grob (stem_tremolo_);
      stem_tremolo_ = 0;
    }
  
}



ENTER_DESCRIPTION(Chord_tremolo_engraver,
/* descr */       "Generates beams for  tremolo repeats.",
/* creats*/       "Beam",
/* accepts */     "general-music",
/* acks  */      "stem-interface note-head-interface",
/* reads */       "",
/* write */       "");
