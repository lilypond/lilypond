/*
  slur-grav.cc -- implement Slur_engraver

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "proto.hh"
#include "musical-request.hh"
#include "slur-engraver.hh"
#include "slur.hh"
#include "debug.hh"
#include "note-column.hh"
#include "translator-group.hh"

bool
Slur_engraver::do_try_music (Music *req_l)
{
  if (Span_req *sl = dynamic_cast <Span_req *> (req_l))
    {
      if (sl->span_type_str_ != "slur")
	return false;
      new_slur_req_l_arr_.push (sl);

      return true;
    }
  return false;
}

void
Slur_engraver::set_melisma (bool m)
{
  Translator_group *where = daddy_trans_l_;
  get_property ("slurMelismaBusy", &where);
  if (!where)
    where = daddy_trans_l_;
    
  daddy_trans_l_->set_property ("slurMelismaBusy", m ? SCM_BOOL_T :SCM_BOOL_F);
}

void
Slur_engraver::acknowledge_element (Score_element_info info)
{
  if (dynamic_cast<Note_column *> (info.elem_l_))
    {
      Note_column *col_l =dynamic_cast<Note_column *> (info.elem_l_) ;// ugh
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
  SCM wg = get_property ("weAreGraceContext",0);
  bool wgb = gh_boolean_p (wg) && gh_scm2bool (wgb);
  if (!wgb)
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
      Span_req* slur_req_l = new_slur_req_l_arr_[i];
      // end slur: move the slur to other array
      if (slur_req_l->span_dir_ == STOP)
	{
	  if (slur_l_stack_.empty())

	    slur_req_l->warning (_f ("Can't find both ends of %s", _("slur")));
	  else
	    {
	      end_slur_l_arr_.push (slur_l_stack_.pop());
	      requests_arr_.pop();
	    }
	}
      else  if (slur_req_l->span_dir_ == START)
	{
	  // push a new slur onto stack.
	  //(use temp. array to wait for all slur STOPs)
	  Slur * s_p =new Slur;
	  SCM prop = get_property ("slurDash", 0);
	  if (SCM_NUMBERP(prop)) 
	    s_p->set_elt_property (dashed_scm_sym, prop);

	  
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
  SCM dir (get_property ("slurVerticalDirection", 0));
  SCM dir2 (get_property ("verticalDirection", 0));

  Direction slurdir = CENTER;
  if (SCM_NUMBERP(dir))
    slurdir = to_dir (dir);
  else if (gh_number_p (dir2))
    slurdir = to_dir (dir2);
  
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
  SCM m = get_property ("automaticMelismata",0);
  if (gh_boolean_p (m) && gh_scm2bool (m))
    {
      set_melisma (slur_l_stack_.size ());
    }
}


ADD_THIS_TRANSLATOR(Slur_engraver);
