/*
  slur-grav.cc -- implement Slur_engraver

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "musical-request.hh"
#include "slur-engraver.hh"
#include "slur.hh"
#include "debug.hh"
#include "note-column.hh"

bool
Slur_engraver::do_try_request (Request *req_l)
{
  Musical_req *mus_l = req_l->access_Musical_req ();
  if (!mus_l || !mus_l->access_Slur_req ())
    return false;

  new_slur_req_l_arr_.push (mus_l->access_Slur_req ());
  return true;
}

void
Slur_engraver::acknowledge_element (Score_element_info info)
{
  if (info.elem_l_->is_type_b (Note_column::static_name ()))
    {
      Note_column *col_l =(Note_column*) info.elem_l_->access_Item() ;// ugh
      for (int i = 0; i < slur_l_stack_.size(); i++)
	slur_l_stack_[i]->add_column (col_l);
      for (int i = 0; i < end_slur_l_arr_.size(); i++)
	end_slur_l_arr_[i]->add_column (col_l);
    }
}

void
Slur_engraver::do_removal_processing ()
{
  for (int i = 0; i < slur_l_stack_.size(); i++)
    {
      typeset_element (slur_l_stack_[i]);
    }
  slur_l_stack_.clear ();
  for (int i=0; i < requests_arr_.size(); i++)
    {
      requests_arr_[i]->warning (_ ("unterminated slur"));
    }
}

void
Slur_engraver::do_process_requests()
{
  Array<Slur*> start_slur_l_arr_;
  for (int i=0; i< new_slur_req_l_arr_.size(); i++)
    {
      Slur_req* slur_req_l = new_slur_req_l_arr_[i];
      // end slur: move the slur to other array
      if (slur_req_l->spantype == Span_req::STOP)
	{
	  if (slur_l_stack_.empty())

	    slur_req_l->warning (_f ("can't find both ends of %s", _("slur")));
	  else
	    {
	      end_slur_l_arr_.push (slur_l_stack_.pop());
	      requests_arr_.pop();
	    }
	}
      else  if (slur_req_l->spantype == Span_req::START)
	{
	  // push a new slur onto stack.
	  //(use temp. array to wait for all slur STOPs)
	  Slur * s_p =new Slur;
	  Scalar prop = get_property ("slurdash");
	  if (prop.isnum_b ()) 
	    s_p->dash_i_ = prop;
	  requests_arr_.push (slur_req_l);
	  start_slur_l_arr_.push (s_p);
	  announce_element (Score_element_info (s_p, slur_req_l));
	}
    }
  for (int i=0; i < start_slur_l_arr_.size(); i++)
    slur_l_stack_.push (start_slur_l_arr_[i]);
}

void
Slur_engraver::do_pre_move_processing()
{
  Scalar dir (get_property ("slurydirection"));
  Scalar dir2 (get_property ("ydirection"));

  Direction slurdir = CENTER;
  if (dir.length_i () && dir.isnum_b ())
    slurdir = (Direction) sign (int(dir));
  else if (dir2.length_i () && dir2.isnum_b ())
    slurdir = (Direction) sign (int (dir2));
  
  for (int i = 0; i < end_slur_l_arr_.size(); i++)
    {
      end_slur_l_arr_[i]->dir_ = slurdir;
      typeset_element (end_slur_l_arr_[i]);
    }
  end_slur_l_arr_.clear();
}

void
Slur_engraver::do_post_move_processing()
{
  new_slur_req_l_arr_.clear();
}

IMPLEMENT_IS_TYPE_B1(Slur_engraver,Engraver);
ADD_THIS_TRANSLATOR(Slur_engraver);
