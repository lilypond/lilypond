/*   
  new-chord-tremolo-engraver.cc --  implement Chord_tremolo_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "beam.hh"
#include "repeated-music.hh"
#include "stem.hh"
#include "note-head.hh"
#include "engraver-group-engraver.hh"
#include "musical-request.hh"
#include "warn.hh"
#include "misc.hh"

/**
  This acknowledges repeated music with "tremolo" style.  It typesets
  a beam.

  TODO:

  - perhaps use engraver this to steer other engravers? That would
  create dependencies between engravers, which is bad.

  - create dots if appropriate.

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
  
  Beam * beam_p_;
  Beam * finished_beam_p_;
  
protected:
  virtual void do_removal_processing();
  virtual void do_process_music();
  virtual bool do_try_music (Music*);
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
};

Chord_tremolo_engraver::Chord_tremolo_engraver()
{
  beam_p_  = finished_beam_p_ = 0;
  repeat_ =0;
  note_head_i_ = 0;
}

bool
Chord_tremolo_engraver::do_try_music (Music * m)
{
  Repeated_music * rp = dynamic_cast<Repeated_music*> (m);
  if (rp && rp->type_ == "tremolo" && !repeat_) 
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
Chord_tremolo_engraver::do_process_music ()
{
  if (repeat_ && !beam_p_)
    {
      beam_p_ = new Beam (get_property ("basicBeamProperties"));
      beam_p_->set_elt_property ("chord-tremolo", SCM_BOOL_T);


      SCM smp = get_property ("measurePosition");
      Moment mp =  (unsmob_moment (smp)) ? *unsmob_moment (smp) : Moment (0);
      beam_start_location_ = mp;
      announce_element (Score_element_info (beam_p_, repeat_));
    }
}


void
Chord_tremolo_engraver::do_removal_processing ()
{
  typeset_beam ();
  if (beam_p_)
    {
      repeat_->warning (_ ("unterminated chord tremolo"));
      finished_beam_p_ = beam_p_;
      typeset_beam ();
    }
}

void
Chord_tremolo_engraver::typeset_beam ()
{
  if (finished_beam_p_)
    {
      typeset_element (finished_beam_p_);
      finished_beam_p_ = 0;
    }
}


void
Chord_tremolo_engraver::acknowledge_element (Score_element_info info)
{
  if (beam_p_)
    {
      if (Stem* s = dynamic_cast<Stem *> (info.elem_l_))
	{
	  int f = s->flag_i ();
	  f = (f > 2) ? f - 2 : 1;
	  s->set_beaming (f, LEFT);
	  s->set_beaming (f, RIGHT);
	  
	  /*
	    URG: this sets the direction of the Stem s.
	    It's amazing Mike:
	    
	      Stem:: type_i () ->first_head ()->get_direction () ->
	              directional_element (me).set (d);


	      don't understand this comment.
		      --hwn.
	   */
	  SCM d = s->get_elt_property ("direction");
	  if (s->type_i () != 1)
	    {
	      int gap_i =s->flag_i () - ((s->type_i () >? 2) - 2);
	      beam_p_->set_elt_property ("beam-gap", gh_int2scm(gap_i));
	    }
	  s->set_elt_property ("direction", d);

	  if (Rhythmic_req* r = dynamic_cast <Rhythmic_req *> (info.req_l_))
	    {
	      beam_p_->add_stem (s);
	      Moment stem_location = now_mom () -
		start_mom_ + beam_start_location_;
	    }
	  else
	    {
	      String s = _ ("stem must have Rhythmic structure");
	      if (info.req_l_)
		info.req_l_->warning (s);
	      else
		::warning (s);
	    }
	}
      if (Note_head *nh = dynamic_cast<Note_head*> (info.elem_l_))
	{
	  nh->set_elt_property ("duration-log", gh_int2scm (intlog2 (note_head_i_)));
	}
    }
}


void
Chord_tremolo_engraver::do_post_move_processing ()
{
  if (beam_p_ && stop_mom_ == now_mom ())
    {
      finished_beam_p_ = beam_p_;

      repeat_ = 0;
      beam_p_ = 0;
    }
}


void
Chord_tremolo_engraver::do_pre_move_processing ()
{
  typeset_beam ();
}

ADD_THIS_TRANSLATOR(Chord_tremolo_engraver);

