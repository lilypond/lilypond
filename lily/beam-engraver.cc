/*   
  beam-engraver.cc --  implement Beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "beam-engraver.hh"
#include "musical-request.hh"
#include "beam.hh"
#include "grouping.hh"
#include "stem.hh"
#include "warn.hh"
#include "time-description.hh"

Beam_engraver::Beam_engraver ()
{
  beam_p_ = 0;
  finished_beam_p_ =0;
  finished_grouping_p_ = 0;
  grouping_p_ =0;
  reqs_drul_[LEFT] = reqs_drul_[RIGHT] =0;
}

bool
Beam_engraver::do_try_music (Music *m)
{
  if (Beam_req * c = dynamic_cast<Beam_req*>(m))
    {
      Direction d =c->spantype_;

      if (d == STOP && !beam_p_)
	{
	  m->warning (_ ("No Beam to end"));
	  return false;
	}
      reqs_drul_[d ] = c;
      return true;
    }
  return false;
}


void
Beam_engraver::do_process_requests ()
{
  if (reqs_drul_[STOP])
    {
      if (!beam_p_)
	reqs_drul_[STOP]->warning (_("No beam to end"));
      finished_beam_p_ = beam_p_;
      finished_grouping_p_ = grouping_p_;

      beam_p_ = 0;
      grouping_p_ = 0;
    }
  
  if (reqs_drul_[START])
    {
      if (beam_p_)
	{
	  reqs_drul_[START]->warning (_ ("Already have a Beam"));
	  return;
	}

      beam_p_ = new Beam;
      grouping_p_ = new Rhythmic_grouping;

      Scalar prop = get_property ("beamslopedamping", 0);
      if (prop.isnum_b ()) 
	beam_p_->damping_i_ = prop;

      prop = get_property ("beamquantisation", 0);
      if (prop.isnum_b ()) 
	beam_p_->quantisation_ = (Beam::Quantisation)(int)prop;
 
      // must set minVerticalAlign == maxVerticalAlign to get sane results
      // see input/test/beam-interstaff.ly
      prop = get_property ("minVerticalAlign", 0);
      if (prop.isnum_b ())
	beam_p_->vertical_align_drul_[MIN] = prop;

      prop = get_property ("maxVerticalAlign", 0);
      if (prop.isnum_b ())
	beam_p_->vertical_align_drul_[MAX] = prop;

      announce_element (Score_element_info (beam_p_, reqs_drul_[START]));
    }
}

void
Beam_engraver::typeset_beam ()
{
  if (finished_beam_p_)
    {
      Rhythmic_grouping const * rg_C = get_staff_info().rhythmic_C_;
      rg_C->extend (finished_grouping_p_->interval());
      finished_beam_p_->set_grouping (*rg_C, *finished_grouping_p_);
      typeset_element (finished_beam_p_);
      finished_beam_p_ = 0;
    
      delete finished_grouping_p_;
      finished_grouping_p_= 0;
    
      reqs_drul_[STOP] = 0;
    }
}

void
Beam_engraver::do_post_move_processing ()
{
  reqs_drul_ [START] =0;
}

void
Beam_engraver::do_pre_move_processing ()
{
  typeset_beam ();
}

void
Beam_engraver::do_removal_processing ()
{
  typeset_beam ();
  if (beam_p_)
    {
      warning (_ ("Unfinished beam"));
      finished_beam_p_ = beam_p_;
      finished_grouping_p_ = grouping_p_;
      typeset_beam ();
    }
}

void
Beam_engraver::acknowledge_element (Score_element_info info)
{
    if (beam_p_)
      {
	Stem* stem_l = dynamic_cast<Stem *> (info.elem_l_);
	if (!stem_l)
	  return;


	Rhythmic_req *rhythmic_req = dynamic_cast <Rhythmic_req *> (info.req_l_);
	if (!rhythmic_req)
	  {
	    String s = _ ("Stem must have Rhythmic structure.");
	    if (info.req_l_)
	      info.req_l_->warning (s);
	    else
	      ::warning (s);
	  
	    return;
	  }
      

	if (rhythmic_req->duration_.durlog_i_<= 2)
	  {
	    rhythmic_req->warning (_ ("stem doesn't fit in beam"));
	    reqs_drul_[LEFT]->warning (_ ("beam was started here"));
	    return;
	  }

	/*
	  TODO: do something sensible if it doesn't fit in the beam.
	*/
	Moment start = get_staff_info().time_C_->whole_in_measure_;

	if (!grouping_p_->child_fit_b (start))
	  {
	    String s (_ ("please fix me") + ": " 
		      + _f ("stem at %s doesn't fit in beam", now_moment ().str ()));

	    if (info.req_l_)
	      info.req_l_->warning(s);
	    else 
	      warning (s);
	  }
	else
	  {
	    grouping_p_->add_child (start, rhythmic_req->duration ());
	    stem_l->flag_i_ = rhythmic_req->duration_.durlog_i_;
	    beam_p_->add_stem (stem_l);
	  }
      }
}



ADD_THIS_TRANSLATOR(Beam_engraver);
