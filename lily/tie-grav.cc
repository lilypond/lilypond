/*
  tie-reg.cc -- implement Tie_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "tie-grav.hh"
#include "tie.hh"
#include "note-head.hh"
#include "musical-request.hh"
#include "music-list.hh"

Tie_engraver::Tie_engraver()
{
  end_tie_p_ = 0;
  tie_p_ = 0;
  req_l_ =0;
  end_req_l_ =0;
  end_mom_ = -1;
  melodic_req_l_ = 0;
  end_melodic_req_l_ =0;
  dir_ = CENTER;
}

void
Tie_engraver::do_post_move_processing()
{
  if (tie_p_ && now_moment () == end_mom_)
    {
      end_tie_p_ = tie_p_;
      end_req_l_ = req_l_;
      end_melodic_req_l_ = melodic_req_l_;
      tie_p_ =0;
      req_l_ =0;
      end_mom_ = -1;
    }
}

bool
Tie_engraver::do_try_request (Request*r)
{
  if (! (r->musical() && r->musical ()->tie ()))
    return false;
  
  if (req_l_)
    {
      return false;
    }
  req_l_ = r->musical()->tie ();
  end_mom_ = r->parent_music_l_->time_int().length ()
    + now_moment ();
  return true;
}

void
Tie_engraver::do_process_requests()
{
  dir_ = (Direction) int (get_property ("ydirection"));
  if (req_l_ && ! tie_p_)
    {
      tie_p_ = new Tie;
      Scalar prop = get_property ("slurdash");
      if (prop.isnum_b ()) 
	tie_p_->dash_i_ = prop;
    }
}

void
Tie_engraver::acknowledge_element (Score_elem_info i)
{
  if (i.elem_l_->is_type_b (Note_head::static_name ()))
    {
      if (tie_p_)
	{
	  tie_p_->set_head (LEFT, (Note_head*)i.elem_l_->item());
	  melodic_req_l_ = i.req_l_->musical()->melodic ();
	}

      if (end_tie_p_)
	{
	  end_tie_p_->set_head (RIGHT, (Note_head*)i.elem_l_->item());
	  if (!Melodic_req::compare (*end_melodic_req_l_, *melodic_req_l_))
	    end_tie_p_->same_pitch_b_ = true;
	  announce_element (Score_elem_info (end_tie_p_,end_req_l_));
	}
    }
}

void
Tie_engraver::do_pre_move_processing()
{
  if (end_tie_p_)
    {
      if (dir_)
	end_tie_p_->dir_ =  dir_;

      typeset_element (end_tie_p_);
      end_tie_p_ =0;
      end_req_l_ =0;
    }
}

void
Tie_engraver::do_removal_processing ()
{
  do_pre_move_processing ();
  if (tie_p_)
    {
      req_l_->warning (_("unended Tie"));
      tie_p_->unlink ();
      delete tie_p_;
    }
}



IMPLEMENT_IS_TYPE_B1(Tie_engraver,Engraver);
ADD_THIS_TRANSLATOR(Tie_engraver);
