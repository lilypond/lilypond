/*
  rhythmic-column-grav.cc -- implement Rhythmic_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "rhythmic-column-grav.hh"
#include "note-head.hh"
#include "stem.hh"
#include "note-column.hh"
#include "script.hh"
#include "dot-column.hh"

Rhythmic_column_engraver::Rhythmic_column_engraver()
{
  stem_l_ =0;
  ncol_p_=0;
  dotcol_p_ =0;
}


void
Rhythmic_column_engraver::acknowledge_element (Score_elem_info i)
{
  if (i.elem_l_->is_type_b (Script::static_name ())
      && i.req_l_ && i.req_l_->musical ()) 
    {
      script_l_arr_.push ((Script*)i.elem_l_->item());
    }
  else if (i.elem_l_->is_type_b (Stem::static_name()))
    {
      stem_l_ = (Stem*) i.elem_l_->item();
    }
  else if (i.elem_l_->is_type_b (Rhythmic_head::static_name ()))
    {
      Rhythmic_head * r = (Rhythmic_head*)i.elem_l_->item ();
      if (!ncol_p_)
	{
	  ncol_p_ = new Note_column;
	  announce_element (Score_elem_info (ncol_p_, 0));
	}
      ncol_p_->add (r);

      if (r->dots_l_)
	{
	  if (!dotcol_p_)
	    {
	      dotcol_p_ = new Dot_column;
	      ncol_p_->set (dotcol_p_);
	      announce_element (Score_elem_info (dotcol_p_, 0));
	    }
	  dotcol_p_->add (r);
	}
    }
  
  if (ncol_p_)
    {
      if (stem_l_&& !ncol_p_->stem_l_) 
	ncol_p_->set (stem_l_);
  
      for (int i=0; i < script_l_arr_.size(); i++) 
	{
	  if (ncol_p_)
	    ncol_p_->add (script_l_arr_[i]);
	}
  
      script_l_arr_.clear();
    }
}

void
Rhythmic_column_engraver::do_pre_move_processing()
{
  if (ncol_p_) 
    {
      if (! ncol_p_->h_shift_b_)
	// egcs
	ncol_p_->h_shift_b_  = get_property ("hshift").operator bool ();
      if (! ncol_p_->dir_)
	ncol_p_->dir_ =(Direction) int(get_property ("ydirection"));

      typeset_element (ncol_p_);
      ncol_p_ =0;
    }
  if (dotcol_p_)
    {
      typeset_element (dotcol_p_);
      dotcol_p_ =0;
    }
}

void
Rhythmic_column_engraver::do_post_move_processing()
{
  script_l_arr_.clear();
  stem_l_ =0;
}




IMPLEMENT_IS_TYPE_B1(Rhythmic_column_engraver,Engraver);
ADD_THIS_TRANSLATOR(Rhythmic_column_engraver);
