/*   
  auto-beam-engraver.cc --  implement Auto_beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "auto-beam-engraver.hh"
#include "musical-request.hh"
#include "beam.hh"
#include "grouping.hh"
#include "rest.hh"
#include "stem.hh"
#include "debug.hh"
#include "time-description.hh"

ADD_THIS_TRANSLATOR (Auto_beam_engraver);

Auto_beam_engraver::Auto_beam_engraver ()
{
  beam_p_ = 0;
  finished_beam_p_ = 0;
  finished_grouping_p_ = 0;
  grouping_p_ = 0;
}

void
Auto_beam_engraver::do_process_requests ()
{
  Time_description const *time = get_staff_info().time_C_;

  //Moment begin_mom;
  Real begin_f;
  Scalar begin = get_property ("beamAutoBegin", 0);
  if (begin.isnum_b ())
    // brrr.
    // begin_mom = (Real)begin;
    begin_f = (Real)begin;
  else
    return;
  
  Scalar end = get_property ("beamAutoEnd", 0);
  //Moment end_mom;
  Real end_f;
  if (end.isnum_b ())
    // brrr.
    // end_mom = (Real)end;
    end_f = (Real)end;
  else
    return;
  
  Real epsilon_f = Moment (1, 512);
  Real f;
  if (beam_p_ && ((f=abs (fmod (time->whole_in_measure_, end_f))) < epsilon_f))
    {
      DOUT << String ("ending autobeam at: ") + now_moment ().str ();
      if (beam_p_->stems_.size () < 2)
	{
	  DOUT << "junking autombeam: less than two stems";
	  unbeam ();
	}
      else
	{
	  finished_beam_p_ = beam_p_;
	  finished_grouping_p_ = grouping_p_;
	  beam_p_ = 0;
	  grouping_p_ = 0;
	}
    }
  
  /*
    Allow already started autobeam to end
   */
  Scalar on = get_property ("beamAuto", 0);
  if (!on.to_bool ())
    return;

  if (!beam_p_ && ((f=abs (fmod (time->whole_in_measure_, begin_f))) < epsilon_f))
    {
      DOUT << String ("starting autobeam at: ") + now_moment ().str ();
      beam_p_ = new Beam;
      grouping_p_ = new Rhythmic_grouping;

      /* urg, copied from Beam_engraver */
      Scalar prop = get_property ("beamslopedamping", 0);
      if (prop.isnum_b ()) 
	beam_p_->damping_i_ = prop;

      prop = get_property ("beamquantisation", 0);
      if (prop.isnum_b ()) 
	beam_p_->quantisation_ = (Beam::Quantisation)(int)prop;
 
      // must set minVerticalAlign = = maxVerticalAlign to get sane results
      // see input/test/beam-interstaff.ly
      prop = get_property ("minVerticalAlign", 0);
      if (prop.isnum_b ())
	beam_p_->vertical_align_drul_[MIN] = prop;

      prop = get_property ("maxVerticalAlign", 0);
      if (prop.isnum_b ())
	beam_p_->vertical_align_drul_[MAX] = prop;

      announce_element (Score_element_info (beam_p_, 0));
    }
}

void
Auto_beam_engraver::typeset_beam ()
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
    }
}

void
Auto_beam_engraver::do_post_move_processing ()
{
}

void
Auto_beam_engraver::do_pre_move_processing ()
{
  typeset_beam ();
}

void
Auto_beam_engraver::do_removal_processing ()
{
  typeset_beam ();
  if (beam_p_)
    {
      DOUT << "Unfinished beam";
      unbeam ();
    }
}

void
Auto_beam_engraver::acknowledge_element (Score_element_info info)
{
  if (Beam *b = dynamic_cast<Beam *> (info.elem_l_))
    {
      if (beam_p_)
	{
	  DOUT << "junking autobeam: beam encountered";
	  unbeam ();
	}
    }

  if (beam_p_)
    {
      Rhythmic_req *rhythmic_req = dynamic_cast <Rhythmic_req *> (info.req_l_);
      if (!rhythmic_req)
	return;

      if (dynamic_cast<Rest *> (info.elem_l_))
	{
	  DOUT << "junking autobeam: rest encountered";
	  unbeam ();
	  return;
	}

      Stem* stem_l = dynamic_cast<Stem *> (info.elem_l_);
      if (!stem_l)
	return;

      if (stem_l->beam_l_ && (stem_l->beam_l_ != beam_p_))
	{
	  DOUT << "junking autobeam: beamed stem encountered";
	  unbeam ();
	  return;
	}
	

      /*
	now that we have last_add_mom_, perhaps we can (should) do away
	with these individual unbeams
       */
      if (rhythmic_req->duration_.durlog_i_<= 2)
	{
	  DOUT << "stem doesn't fit in beam";
	  unbeam ();
	  return;
	}

      Moment start = get_staff_info().time_C_->whole_in_measure_;
      if (!grouping_p_->child_fit_b (start))
	{
	  unbeam ();
	}
      else
	{
	  grouping_p_->add_child (start, rhythmic_req->duration ());
	  stem_l->flag_i_ = rhythmic_req->duration_.durlog_i_;
	  beam_p_->add_stem (stem_l);
	  Moment now = now_moment ();
	  last_add_mom_ = now;
	  extend_mom_ = extend_mom_ >? now + rhythmic_req->duration ();
	}
    }
}

void
Auto_beam_engraver::unbeam () 
{
  assert (beam_p_);
  for (int i=0; i < beam_p_->stems_.size (); i++)
    {
      Stem* s = beam_p_->stems_[i];
      s->beams_i_drul_[LEFT] = 0;
      s->beams_i_drul_[RIGHT] = 0;
      s->mult_i_ = 0;
      s->beam_l_ = 0;
    }
  
  beam_p_->unlink ();
  beam_p_ = 0;
  delete grouping_p_;
  grouping_p_ = 0;
}

void
Auto_beam_engraver::process_acknowledged ()
{
  if (beam_p_)
    {
      Moment now = now_moment ();
      if ((extend_mom_ < now)
	  || ((extend_mom_ == now) && (last_add_mom_ != now )))
	{
	  DOUT << String ("junking autobeam: no stem added since: ")
	    + last_add_mom_.str ();
	  unbeam ();
	}
      else if (!beam_p_->stems_.size ())
	{
	  DOUT << "junking started autobeam: no stems";
	  unbeam ();
	}
    }
}
