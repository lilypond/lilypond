/*
  chord-tremolo-engraver.cc -- implement Chord_tremolo_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
           Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "duration-convert.hh"
#include "timing-translator.hh"
#include "chord-tremolo-engraver.hh"
#include "stem.hh"
#include "beam.hh"
#include "musical-request.hh"
#include "misc.hh"
#include "warn.hh"
#include "score-engraver.hh"

ADD_THIS_TRANSLATOR (Chord_tremolo_engraver);

Chord_tremolo_engraver::Chord_tremolo_engraver ()
{
  reqs_drul_[LEFT] = reqs_drul_[RIGHT] = 0;
  abeam_p_ = 0;
  finished_abeam_p_ = 0;
  prev_start_req_ = 0;
}

bool
Chord_tremolo_engraver::do_try_music (Music* m)
{
  if (Chord_tremolo_req* b = dynamic_cast <Chord_tremolo_req *> (m))
    {
      Direction d = b->span_dir_;
      if (reqs_drul_[d] && !reqs_drul_[d]->equal_b (b))
	return false;

      if ((d == STOP) && !abeam_p_)
	{
	  m->warning (_ ("no tremolo beam to end"));
	  return false;
	}

      reqs_drul_[d] = b;
      return true;
    }

  return false;
}

void
Chord_tremolo_engraver::do_process_requests ()
{
  if (reqs_drul_[STOP])
    {
      if (!abeam_p_)
	reqs_drul_[STOP]->warning (_ ("no tremolo beam to end"));
      prev_start_req_ = 0;
      finished_abeam_p_ = abeam_p_;
      abeam_p_ = 0;
    }

  if (abeam_p_)
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
      if (abeam_p_)
	{
	  reqs_drul_[START]->warning (_ ("Already have a tremolo beam"));
	  return;
	}

      prev_start_req_ = reqs_drul_[START];

      abeam_p_ = new Beam;
      abeam_p_->set_elt_property ("chord-tremolo", SCM_BOOL_T);
      
      announce_element (Score_element_info (abeam_p_, reqs_drul_[LEFT]));
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
  if (finished_abeam_p_)
    {
      typeset_element (finished_abeam_p_);
      finished_abeam_p_ = 0;

      reqs_drul_[STOP] = 0;
    }
}

void
Chord_tremolo_engraver::do_removal_processing ()
{
  typeset_beam ();
  if (abeam_p_)
    {
      prev_start_req_->warning (_ ("unfinished tremolo beam"));
      finished_abeam_p_ = abeam_p_;
      typeset_beam ();
    }
}

void
Chord_tremolo_engraver::acknowledge_element (Score_element_info i)
{
  if (abeam_p_)
    {
      if (Stem* s = dynamic_cast<Stem *> (i.elem_l_))
	{
	  int type_i = prev_start_req_->type_i_;
	  s->set_elt_property ("duration-log",  gh_int2scm (intlog2 (type_i) - 2));

	  s->beams_i_drul_[LEFT] = s->flag_i ();
	  s->beams_i_drul_[RIGHT] = s->flag_i ();
	  
	  abeam_p_->multiplicity_i_ = s->flag_i ();
	  /*
	    abbrev gaps on all but half note
	  */
#if 0
	  if (s->type_i () != 1)
	    {
	      int gap_i =s->flag_i () - ((s->type_i () >? 2) - 2);
	      s->set_elt_property ("beam-gap", gh_int2scm(gap_i));
	    }
#else
	  if (s->type_i () != 1)
	    {
	      int gap_i =s->flag_i () - ((s->type_i () >? 2) - 2);
	      abeam_p_->set_elt_property ("beam-gap", gh_int2scm(gap_i));
	    }
#endif
	  
	  abeam_p_->add_stem (s);
	}
    }
}

