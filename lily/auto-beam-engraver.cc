/*   
  auto-beam-engraver.cc --  implement Auto_beam_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */

#include "auto-beam-engraver.hh"
#include "musical-request.hh"
#include "bar.hh"
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

/*
  should move this to rational, but may reject now
  */
Rational
str2rat (String str)
{
  int num;
  int den = 1;
  if (int i = str.index_i ('/') != -1)
    {
      den = str.cut_str (i + 1, str.length_i ()).value_i ();
      str = str.left_str (i);
    }
  num = str.value_i ();
  return Rational (num, den);
}

void
Auto_beam_engraver::do_process_requests ()
{
  Time_description const *time = get_staff_info().time_C_;

  Scalar begin = get_property ("beamAutoBegin", 0);
  Moment begin_mom = str2rat (begin);
  
  Scalar end = get_property ("beamAutoEnd", 0);
  Moment end_mom = str2rat (end);

  if (mult_i_)
    {
      Scalar end_mult = get_property (String ("beamAutoEnd")
				      + to_str (1 << (mult_i_ + 2)), 0);
      if (end_mult.length_i ())
	end_mom = str2rat (end_mult);
      else if (end_mom / Moment (mult_i_, 1) > Moment (4))
	end_mom /= Moment (mult_i_);
    }

  Real f;
  if (end_mom)
    f = fmod (time->whole_in_measure_, end_mom);
  else
    f = Moment (1);

  // enge floots
  Real epsilon_f = Moment (1, 512);
  if (beam_p_ && (abs (f) < epsilon_f))
    end_beam ();
     
  /*
    Allow already started autobeam to end
   */
  Scalar on = get_property ("beamAuto", 0);
  if (!on.to_bool ())
    return;

  if (begin_mom)
    f = fmod (time->whole_in_measure_, begin_mom);
  if (!beam_p_ && (!begin_mom || (abs (f) < epsilon_f)))
    begin_beam ();
}

      
void
Auto_beam_engraver::begin_beam ()
{
  DOUT << String ("starting autobeam at: ") + now_moment ().str () + "\n";
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

void
Auto_beam_engraver::end_beam ()
{
  DOUT << String ("ending autobeam at: ") + now_moment ().str () + "\n";
  if (beam_p_->stems_.size () < 2)
    {
      DOUT << "junking autombeam: less than two stems\n";
      junk_beam ();
    }
  else
    {
      finished_beam_p_ = beam_p_;
      finished_grouping_p_ = grouping_p_;
      beam_p_ = 0;
      grouping_p_ = 0;
      mult_i_ = 0;
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
      DOUT << "Unfinished beam\n";
      junk_beam ();
    }
}

void
Auto_beam_engraver::acknowledge_element (Score_element_info info)
{
  if (Beam *b = dynamic_cast<Beam *> (info.elem_l_))
    {
      if (beam_p_)
	{
	  DOUT << "junking autobeam: beam encountered\n";
	  junk_beam ();
	}
    }
  if (Bar *b = dynamic_cast<Bar *> (info.elem_l_))
    {
      if (beam_p_)
	{
	  DOUT << "junking autobeam: bar encountered\n";
	  junk_beam ();
	}
    }

  if (beam_p_)
    {
      Rhythmic_req *rhythmic_req = dynamic_cast <Rhythmic_req *> (info.req_l_);
      if (!rhythmic_req)
	return;

      if (dynamic_cast<Rest *> (info.elem_l_))
	{
	  DOUT << "junking autobeam: rest encountered\n";
	  end_beam ();
	  return;
	}

      Stem* stem_l = dynamic_cast<Stem *> (info.elem_l_);
      if (!stem_l)
	return;

      if (stem_l->beam_l_ && (stem_l->beam_l_ != beam_p_))
	{
	  DOUT << "junking autobeam: beamed stem encountered\n";
	  junk_beam ();
	  return;
	}
	

      /*
	now that we have last_add_mom_, perhaps we can (should) do away
	with these individual junk_beams
       */
      if (rhythmic_req->duration_.durlog_i_<= 2)
	{
	  DOUT << "ending autobeam: stem doesn't fit in beam\n";
	  end_beam ();
	  return;
	}

      Moment start = get_staff_info().time_C_->whole_in_measure_;
      if (!grouping_p_->child_fit_b (start))
	{
	  DOUT << "ending autobeam: stem doesn't fit in group\n";
	  end_beam ();
	}
      else
	{
	  grouping_p_->add_child (start, rhythmic_req->duration ());
	  stem_l->flag_i_ = rhythmic_req->duration_.durlog_i_;
	  beam_p_->add_stem (stem_l);
	  Moment now = now_moment ();
	  last_add_mom_ = now;
	  extend_mom_ = extend_mom_ >? now + rhythmic_req->duration ();
	  mult_i_ = mult_i_ >? (rhythmic_req->duration_.durlog_i_ - 2);
	}
    }
}

void
Auto_beam_engraver::junk_beam () 
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
  mult_i_ = 0;
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
	    + last_add_mom_.str () + "\n";
	  end_beam ();
	}
      else if (!beam_p_->stems_.size ())
	{
	  DOUT << "junking started autobeam: no stems\n";
	  junk_beam ();
	}
    }
}
