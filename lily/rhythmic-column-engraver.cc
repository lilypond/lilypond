/*
  rhythmic-column-grav.cc -- implement Rhythmic_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "slur.hh"
#include "engraver.hh"
#include "rhythmic-head.hh"
#include "stem.hh"
#include "note-column.hh"
#include "dot-column.hh"
#include "musical-request.hh"
#include "item.hh"

class Rhythmic_column_engraver :public Engraver
{
  Link_array<Score_element> rhead_l_arr_;
  Link_array<Score_element> grace_slur_endings_;
  Score_element * stem_l_;
  Score_element *ncol_p_;
  Score_element *dotcol_l_;

protected:
  VIRTUAL_COPY_CONS(Translator);
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
public:
  Rhythmic_column_engraver();
  
};



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
	  ncol_p_ = new Item (get_property("basicNoteColumnProperties"));
	  Note_column::set_interface (ncol_p_);
	  announce_element (Score_element_info (ncol_p_, 0));
	}

      for (int i=0; i < rhead_l_arr_.size (); i++)
	{
	  if (!rhead_l_arr_[i]->parent_l(X_AXIS))
	    Note_column::add_head (ncol_p_, rhead_l_arr_[i]);
	}
      rhead_l_arr_.set_size (0);
    }

  
  if (ncol_p_)
    {
      if (dotcol_l_
	  && !dotcol_l_->parent_l (X_AXIS))
	{
	  Note_column::set_dotcol (ncol_p_, dotcol_l_);
	}

      if (stem_l_
	  && !stem_l_->parent_l(X_AXIS))
	{
	  Note_column::set_stem (ncol_p_, stem_l_);
	  stem_l_ = 0;
	}

      SCM wg = get_property ("weAreGraceContext");
      bool wegrace = to_boolean (wg);

      if (!wegrace)
	for (int i=0; i < grace_slur_endings_.size(); i++)
	  Slur::add_column (grace_slur_endings_[i], ncol_p_);
      grace_slur_endings_.clear ();
    }
}

void
Rhythmic_column_engraver::acknowledge_element (Score_element_info i)
{
  SCM wg = get_property ("weAreGraceContext");
  bool wegrace = to_boolean (wg);
  if (wegrace != to_boolean (i.elem_l_->get_elt_property ("grace"))
    && !Slur::has_interface (i.elem_l_))
    return ;
  
  Item * item =  dynamic_cast <Item *> (i.elem_l_);
  if (item && Stem::has_interface (item))
    {
      stem_l_ = item;
    }
  else if (item && Rhythmic_head::has_interface (item))
    {
      rhead_l_arr_.push (item);
    }
  else if (item && Dot_column::has_interface (item))
    {
      dotcol_l_ = item;
    }
  else if (Slur::has_interface (i.elem_l_))
    {
      /*
	end slurs starting on grace notes
       */
      
      if (to_boolean (i.elem_l_->get_elt_property ("grace")))
	grace_slur_endings_.push (i.elem_l_);
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

