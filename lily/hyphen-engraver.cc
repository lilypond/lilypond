/*
  hyphen-engraver.cc -- implement Hyphen_engraver

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>
*/

#include "proto.hh"
#include "musical-request.hh"
#include "hyphen-engraver.hh"
#include "hyphen-spanner.hh"
#include "paper-column.hh"
#include "text-item.hh"
#include "extender-engraver.hh"

ADD_THIS_TRANSLATOR (Hyphen_engraver);

Hyphen_engraver::Hyphen_engraver ()
{
  current_lyric_l_ = 0;
  last_lyric_l_ = 0;
  hyphen_spanner_p_ = 0;
  req_l_ = 0;
}

void
Hyphen_engraver::acknowledge_element (Score_element_info i)
{
  if (Text_item* t = dynamic_cast<Text_item*> (i.elem_l_))
    {
      current_lyric_l_ = t;
      if (hyphen_spanner_p_
	  && !hyphen_spanner_p_->get_bound (RIGHT)
	    )
	  {
	    hyphen_spanner_p_->set_textitem (RIGHT, t);
	  }
    }
}


bool
Hyphen_engraver::do_try_music (Music* r)
{
  if (Hyphen_req* p = dynamic_cast <Hyphen_req *> (r))
    {
      if (req_l_)
	return false;

      req_l_ = p;
      return true;
    }
  return false;
}

void
Hyphen_engraver::do_removal_processing ()
{
  if (hyphen_spanner_p_)
    {
      req_l_->warning (_ ("unterminated hyphen"));
      hyphen_spanner_p_->set_bound(RIGHT, get_staff_info ().command_pcol_l ());
    }
}

void
Hyphen_engraver::do_process_music ()
{
  if (req_l_)
    {
      if (!last_lyric_l_)
	{
	  req_l_->warning (_ ("Nothing to connect hyphen to on the left.  Ignoring hyphen request."));
	  return;
	}
      
      hyphen_spanner_p_ = new Hyphen_spanner (SCM_EOL);
      hyphen_spanner_p_->set_textitem  (LEFT, last_lyric_l_);
      announce_element (Score_element_info (hyphen_spanner_p_, req_l_));
    }
}


void
Hyphen_engraver::do_pre_move_processing ()
{
  if (hyphen_spanner_p_)
    {
      typeset_element (hyphen_spanner_p_);
      hyphen_spanner_p_ = 0;
    }

  if (current_lyric_l_)
    {
      last_lyric_l_ = current_lyric_l_;
      current_lyric_l_ =0;
    }
}

void
Hyphen_engraver::do_post_move_processing ()
{
  req_l_ = 0;
}


