/*
  rhythmic-column-grav.cc -- implement Rhythmic_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dimension-cache.hh"
#include "slur.hh"
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
	  ncol_p_ = new Note_column (SCM_EOL);
	  announce_element (Score_element_info (ncol_p_, 0));
	}

      for (int i=0; i < rhead_l_arr_.size (); i++)
	{
	  if (!rhead_l_arr_[i]->parent_l(X_AXIS))
	    ncol_p_->add_head (rhead_l_arr_[i]);
	}
      rhead_l_arr_.set_size (0);
    }

  
  if (ncol_p_)
    {
      if (dotcol_l_
	  && !dotcol_l_->parent_l(X_AXIS))
	{
	  ncol_p_->set_dotcol (dotcol_l_);
	}

      if (stem_l_
	  && !stem_l_->parent_l(X_AXIS))
	{
	  ncol_p_->set_stem (stem_l_);
	  stem_l_ = 0;
	}

      SCM wg = get_property ("weAreGraceContext");
      bool wegrace = to_boolean (wg);

      if (!wegrace)
	for (int i=0; i < grace_slur_endings_.size(); i++)
	  grace_slur_endings_[i]->add_column (ncol_p_);
      grace_slur_endings_.clear ();
    }
}

void
Rhythmic_column_engraver::acknowledge_element (Score_element_info i)
{
  SCM wg = get_property ("weAreGraceContext");
  bool wegrace = to_boolean (wg);
  if ((wegrace !=
      (i.elem_l_->get_elt_property ("grace") != SCM_UNDEFINED))
    && !dynamic_cast<Slur*> (i.elem_l_))
    return ;
  
  Item * item =  dynamic_cast <Item *> (i.elem_l_);
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
  else if (Slur *s = dynamic_cast<Slur*> (i.elem_l_))
    {
      /*
	end slurs starting on grace notes
       */
      
      if (to_boolean (s->get_elt_property ("grace")))
	grace_slur_endings_.push (s);
   }
}

void
Rhythmic_column_engraver::do_pre_move_processing()
{
  if (ncol_p_) 
    {
      typeset_element (ncol_p_);
      ncol_p_ =0;
    }
}

void
Rhythmic_column_engraver::do_post_move_processing()
{
  grace_slur_endings_.clear ();
  dotcol_l_ =0;
  stem_l_ =0;
}

ADD_THIS_TRANSLATOR(Rhythmic_column_engraver);

