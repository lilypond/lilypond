/*   
  new-chord-tremolo-engraver.cc --  implement Chord_tremolo_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  VIRTUAL_COPY_CONS(Translator);
  Chord_tremolo_engraver();
protected:
  Repeated_music * repeat_;

  /// moment (global time) where beam started.
  Moment start_mom_;
  Moment stop_mom_;

  /// location  within measure where beam started.
  Moment beam_start_location_;

  int note_head_i_;
  
  Spanner * beam_p_;
  Spanner * finished_beam_p_;
  
protected:
  virtual void finalize();
  virtual bool try_music (Music*);
  virtual void acknowledge_grob (Grob_info);
  virtual void stop_translation_timestep();
  virtual void start_translation_timestep();
  virtual void create_grobs ();
};

Chord_tremolo_engraver::Chord_tremolo_engraver()
{
  beam_p_  = finished_beam_p_ = 0;
  repeat_ =0;
  note_head_i_ = 0;
}

bool
Chord_tremolo_engraver::try_music (Music * m)
{
  Repeated_music * rp = dynamic_cast<Repeated_music*> (m);
  if (rp
      && rp->get_mus_property ("iterator-ctor") == Chord_tremolo_iterator::constructor_cxx_function
      && !repeat_) 
    {
      Moment l = rp->body_length_mom ();
      repeat_ = rp;
      start_mom_ = now_mom ();
      stop_mom_ = start_mom_ + l;

      // ugh. should generate dots, triplet beams.      
      note_head_i_ = l.den () <? 4; 
      return true;
    }
  return false;
}

void
Chord_tremolo_engraver::create_grobs ()
{
  if (repeat_ && !beam_p_)
    {
      beam_p_ = new Spanner (get_property ("Beam"));
      Beam::set_interface (beam_p_);
      beam_p_->set_grob_property ("chord-tremolo", SCM_BOOL_T);


      SCM smp = get_property ("measurePosition");
      Moment mp =  (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
      beam_start_location_ = mp;
      announce_grob (beam_p_, repeat_);
    }
}


void
Chord_tremolo_engraver::finalize ()
{
  typeset_beam ();
  if (beam_p_)
    {
      repeat_->origin ()->warning (_ ("unterminated chord tremolo"));
#if 0
      finished_beam_p_ = beam_p_;
      typeset_beam ();
#else
      beam_p_->suicide ();
#endif
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
	  if (Stem::type_i (s ) != 1)
	    {
	      int gap_i =Stem::flag_i (s ) - ((Stem::type_i (s ) >? 2) - 2);
	      beam_p_->set_grob_property ("beam-gap", gh_int2scm(gap_i));
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
      if (Note_head::has_interface (info.elem_l_))
	{
	  info.elem_l_->set_grob_property ("duration-log", gh_int2scm (intlog2 (note_head_i_)));
	}
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
}

ADD_THIS_TRANSLATOR(Chord_tremolo_engraver);

