/*   
  new-chord-tremolo-engraver.cc --  implement Chord_tremolo_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
public:
  VIRTUAL_COPY_CONS (Translator);
  Chord_tremolo_engraver ();
protected:
  Repeated_music * repeat_;

  /// moment (global time) where beam started.
  Moment start_mom_;
  Moment stop_mom_;

  /// location  within measure where beam started.
  Moment beam_start_location_;

  int note_head_i_;

  bool sequential_body_b_;
  Spanner * beam_p_;
  Spanner * finished_beam_p_;
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
  beam_p_  = finished_beam_p_ = 0;
  repeat_ =0;
  note_head_i_ = 0;
  stem_tremolo_ = 0;
  sequential_body_b_ = false;
}

bool
Chord_tremolo_engraver::try_music (Music * m)
{
  Repeated_music * rp = dynamic_cast<Repeated_music*> (m);
  if (rp
      && rp->get_mus_property ("iterator-ctor") == Chord_tremolo_iterator::constructor_cxx_function
      && !repeat_) 
    {
      Moment l = rp->length_mom ();
      repeat_ = rp;
      start_mom_ = now_mom ();
      stop_mom_ = start_mom_ + l;
      sequential_body_b_ = dynamic_cast<Sequential_music*> (rp->body ());

      // ugh. should generate dots, triplet beams.      
      note_head_i_ = l.den () <? 4; 
      return true;
    }

  return false;
}

void
Chord_tremolo_engraver::process_music ()
{
  if (repeat_)
    {
      if (sequential_body_b_ && !beam_p_)
	{
	  beam_p_ = new Spanner (get_property ("Beam"));
	  beam_p_->set_grob_property ("chord-tremolo", SCM_BOOL_T);


	  SCM smp = get_property ("measurePosition");
	  Moment mp
	    = (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
	  beam_start_location_ = mp;
	  announce_grob (beam_p_, repeat_);
	}
      else if (!sequential_body_b_ && !stem_tremolo_)
	{
	  int flags = intlog2 (note_head_i_ * repeat_->repeat_count ()) -2;
	  if (flags)
	    {
	      stem_tremolo_ = new Item (get_property ("StemTremolo"));
	      Stem_tremolo::set_interface (stem_tremolo_);

	      announce_grob (stem_tremolo_, repeat_);
	      stem_tremolo_->set_grob_property ("tremolo-flags",
						gh_int2scm (flags));

	    }
	}
    }
}
void
Chord_tremolo_engraver::finalize ()
{
  typeset_beam ();
  if (beam_p_)
    {
      repeat_->origin ()->warning (_ ("unterminated chord tremolo"));
      beam_p_->suicide ();
    }
}

void
Chord_tremolo_engraver::typeset_beam ()
{
  if (finished_beam_p_)
    {
      typeset_grob (finished_beam_p_);
      finished_beam_p_ = 0;
    }
}


void
Chord_tremolo_engraver::acknowledge_grob (Grob_info info)
{
  if (beam_p_)
    {
      if (Stem::has_interface (info.elem_l_))
	{
	  Grob * s = info.elem_l_;
	  int f = Stem::flag_i (s);
	  f = (f > 2) ? f - 2 : 1;
	  Stem::set_beaming (s, f, LEFT);
	  Stem::set_beaming (s, f, RIGHT);
	  
	  /*
	    URG: this sets the direction of the Stem s.
	    It's amazing Mike:
	    
	      Stem:: type_i () ->first_head ()->get_direction () ->
	              Directional_element_interface::set (me, d);


	      don't understand this comment.
		      --hwn.
	   */
 	  SCM d = s->get_grob_property ("direction");
	  if (Stem::type_i (s) != 1)
	    {
	      int gap_i =Stem::flag_i (s) - ((Stem::type_i (s) >? 2) - 2);
	      beam_p_->set_grob_property ("gap", gh_int2scm (gap_i));
	    }
	  s->set_grob_property ("direction", d);

	  if (dynamic_cast <Rhythmic_req *> (info.req_l_))
	    {
	      Beam::add_stem (beam_p_, s);
	    }
	  else
	    {
	      String s = _ ("stem must have Rhythmic structure");
	      if (info.req_l_)
		info.req_l_->origin ()->warning (s);
	      else
		::warning (s);
	    }
	}
    }
  else if (stem_tremolo_ && Stem::has_interface (info.elem_l_))
    {
       Stem_tremolo::set_stem (stem_tremolo_, info.elem_l_);

       info.elem_l_->set_grob_property ("duration-log", gh_int2scm (intlog2 (note_head_i_)));
    }

  
  if (repeat_ && Note_head::has_interface (info.elem_l_))
    {
      info.elem_l_->set_grob_property ("duration-log", gh_int2scm (intlog2 (note_head_i_)));
    }
}


void
Chord_tremolo_engraver::start_translation_timestep ()
{
  if (beam_p_ && stop_mom_ == now_mom ())
    {
      finished_beam_p_ = beam_p_;

      repeat_ = 0;
      beam_p_ = 0;
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

ADD_THIS_TRANSLATOR (Chord_tremolo_engraver);

