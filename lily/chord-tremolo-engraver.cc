/*
  chord-tremolo-engraver.cc -- implement Chord_tremolo_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "timing-translator.hh"
#include "stem.hh"
#include "beam.hh"
#include "musical-request.hh"
#include "misc.hh"
#include "warn.hh"
#include "score-engraver.hh"
#include "engraver.hh"
#include "drul-array.hh"
#include "timing-engraver.hh"
#include "beaming.hh"

/**
  Generate a beam tremolo.  Eat stems.

  UGH. Derive me from Beam_engraver.
  
 */
class Chord_tremolo_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS(Translator);
  

  Chord_tremolo_engraver();

protected:
  virtual void do_removal_processing();
  virtual void do_process_music();
  virtual bool do_try_music (Music*);
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();

private:
  void typeset_beam ();
  Drul_array<Chord_tremolo_req*> reqs_drul_;
  Chord_tremolo_req* prev_start_req_;
  Beam* beam_p_;
  Beam* finished_beam_p_;

  /// location  within measure where beam started.
  Moment beam_start_location_;

  /// moment (global time) where beam started.
  Moment beam_start_mom_;
  Beaming_info_list * beam_info_p_;
  Beaming_info_list * finished_beam_info_p_;  
};

ADD_THIS_TRANSLATOR (Chord_tremolo_engraver);

Chord_tremolo_engraver::Chord_tremolo_engraver ()
{
  reqs_drul_[LEFT] = reqs_drul_[RIGHT] = 0;
  beam_p_ = 0;
  finished_beam_p_ = 0;
  prev_start_req_ = 0;
  finished_beam_info_p_=0;
  beam_info_p_ =0;
}

bool
Chord_tremolo_engraver::do_try_music (Music* m)
{
  if (Chord_tremolo_req* b = dynamic_cast <Chord_tremolo_req *> (m))
    {
      Direction d = b->span_dir_;
      if (reqs_drul_[d] && !reqs_drul_[d]->equal_b (b))
	return false;

      if ((d == STOP) && !beam_p_)
	{
	  m->warning (_ ("can't find start of chord tremolo"));
	  return false;
	}

      reqs_drul_[d] = b;
      return true;
    }

  return false;
}

void
Chord_tremolo_engraver::do_process_music ()
{
  if (reqs_drul_[STOP])
    {
      if (!beam_p_)
	reqs_drul_[STOP]->warning (_ ("can't find start of chord tremolo"));

      prev_start_req_ = 0;

      finished_beam_p_ = beam_p_;
      beam_p_ = 0;

      finished_beam_info_p_ = beam_info_p_;
      beam_info_p_ = 0;
    }

  if (beam_p_)
    {
      Score_engraver * e = 0;
      Translator * t  =  daddy_grav_l ();
      for (; !e && t;  t = t->daddy_trans_l_)
	{
	  e = dynamic_cast<Score_engraver*> (t);
	}
      
      if (!e)
	programming_error ("No score engraver!");
      else
	e->forbid_breaks ();
    }

  if (reqs_drul_[START])
    {
      if (beam_p_)
	{
	  reqs_drul_[START]->warning (_ ("already have a chord tremolo"));
	  return;
	}

      prev_start_req_ = reqs_drul_[START];

      beam_p_ = new Beam;
      beam_p_->set_elt_property ("chord-tremolo", SCM_BOOL_T);
      Translator * t  = daddy_grav_l  ()->get_simple_translator ("Timing_engraver");
      Timing_engraver *timer = dynamic_cast<Timing_engraver*> (t);
      beam_start_location_ = (t) ?  timer->measure_position () : Moment (0);
      beam_start_mom_ = now_mom();
      beam_info_p_ = new Beaming_info_list;
      
      announce_element (Score_element_info (beam_p_, reqs_drul_[LEFT]));
  }
}

void
Chord_tremolo_engraver::do_post_move_processing ()
{
  reqs_drul_ [START] = 0;
}


void
Chord_tremolo_engraver::do_pre_move_processing ()
{
  typeset_beam ();
}

void
Chord_tremolo_engraver::typeset_beam ()
{
  if (finished_beam_p_)
    {
      finished_beam_info_p_->beamify ();
      finished_beam_p_->set_beaming (finished_beam_info_p_);
      typeset_element (finished_beam_p_);
      finished_beam_p_ = 0;
      delete finished_beam_info_p_;
      finished_beam_info_p_ =0;

      reqs_drul_[STOP] = 0;
    }
}

void
Chord_tremolo_engraver::do_removal_processing ()
{
  typeset_beam ();
  if (beam_p_)
    {
      prev_start_req_->warning (_ ("unterminated chord tremolo"));
      finished_beam_p_ = beam_p_;
      finished_beam_info_p_ = beam_info_p_;
      typeset_beam ();
    }
}

void
Chord_tremolo_engraver::acknowledge_element (Score_element_info info)
{
  if (beam_p_)
    {
      if (Stem* s = dynamic_cast<Stem *> (info.elem_l_))
	{
	  int type_i = prev_start_req_->type_i_;
	  s->set_elt_property ("duration-log",  gh_int2scm (intlog2 (type_i) - 2));

	  s->set_beaming (s->flag_i (), LEFT);
	  s->set_beaming ( s->flag_i (), RIGHT);
	  
	  /*
	    URG: this sets the direction of the Stem s.
	    It's amazing Mike:
	    
	      Stem:: type_i () ->first_head ()->get_direction () ->
	              directional_element (me).set (d);
	    
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
		beam_start_mom_ + beam_start_location_;
	      beam_info_p_->add_stem (stem_location,
				      (r->duration_.durlog_i_ - 2) >? 1);
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
    }
}

