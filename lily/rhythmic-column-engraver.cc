/*
  rhythmic-column-grav.cc -- implement Rhythmic_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "rhythmic-column-engraver.hh"
#include "note-head.hh"
#include "stem.hh"
#include "note-column.hh"
#include "dot-column.hh"
#include "musical-request.hh"

Rhythmic_column_engraver::Rhythmic_column_engraver()
{
  stem_l_ =0;
  ncol_p_=0;
  dotcol_l_ =0;
}


void
Rhythmic_column_engraver::process_acknowledged ()
{
  if (rhead_l_arr_.size ())
    {
      if (!ncol_p_)
	{
	  ncol_p_ = new Note_column;
	  announce_element (Score_element_info (ncol_p_, 0));
	}

      for (int i=0; i < rhead_l_arr_.size (); i++)
	{
	  ncol_p_->add_head (rhead_l_arr_[i]);
	}
      rhead_l_arr_.set_size (0);
    }

  
  if (ncol_p_)
    {
      if (dotcol_l_)
	{
	  ncol_p_->set_dotcol (dotcol_l_);
	}

      if (stem_l_)
	{
	  ncol_p_->set_stem (stem_l_);
	  stem_l_ = 0;
	}
    }
}

void
Rhythmic_column_engraver::acknowledge_element (Score_element_info i)
{
  Item * item =  dynamic_cast <Item *> (i.elem_l_);
  if (!item)
    return;
 if (Stem*s=dynamic_cast<Stem *> (item))
    {
      stem_l_ = s;
    }
      else if (Rhythmic_head*r=dynamic_cast<Rhythmic_head *> (item))
    {
      rhead_l_arr_.push (r);
    }
      else if (Dot_column*d =dynamic_cast<Dot_column *> (item))
    {
      dotcol_l_ = d;
    }
}

void
Rhythmic_column_engraver::do_pre_move_processing()
{
  if (ncol_p_) 
    {
      // egcs
      if (get_property ("hshift", 0).operator bool ())
	{
	  ncol_p_->set_elt_property (horizontal_shift_scm_sym, SCM_BOOL_T);
	}

      typeset_element (ncol_p_);
      ncol_p_ =0;
    }
}

void
Rhythmic_column_engraver::do_post_move_processing()
{
  dotcol_l_ =0;
  stem_l_ =0;
}





ADD_THIS_TRANSLATOR(Rhythmic_column_engraver);
