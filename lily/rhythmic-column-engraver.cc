/*
  rhythmic-column-grav.cc -- implement Rhythmic_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  Link_array<Grob> rhead_l_arr_;
  Link_array<Grob> grace_slur_endings_;
  Grob * stem_l_;
  Grob *ncol_p_;
  Grob *dotcol_l_;

protected:
  VIRTUAL_COPY_CONS (Translator);
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
public:
  Rhythmic_column_engraver ();
  
};



Rhythmic_column_engraver::Rhythmic_column_engraver ()
{
  stem_l_ =0;
  ncol_p_=0;
  dotcol_l_ =0;
}


void
Rhythmic_column_engraver::create_grobs ()
{
  if (rhead_l_arr_.size ())
    {
      if (!ncol_p_)
	{
	  ncol_p_ = new Item (get_property ("NoteColumn"));
	  Note_column::set_interface (ncol_p_);
	  announce_grob (ncol_p_, 0);
	}

      for (int i=0; i < rhead_l_arr_.size (); i++)
	{
	  if (!rhead_l_arr_[i]->parent_l (X_AXIS))
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
	  && !stem_l_->parent_l (X_AXIS))
	{
	  Note_column::set_stem (ncol_p_, stem_l_);
	  stem_l_ = 0;
	}

      SCM wg = get_property ("weAreGraceContext");
      bool wegrace = to_boolean (wg);

      if (!wegrace)
	for (int i=0; i < grace_slur_endings_.size (); i++)
	  Slur::add_column (grace_slur_endings_[i], ncol_p_);
      grace_slur_endings_.clear ();
    }
}

void
Rhythmic_column_engraver::acknowledge_grob (Grob_info i)
{
  SCM wg = get_property ("weAreGraceContext");
  bool wegrace = to_boolean (wg);
  if (wegrace != to_boolean (i.elem_l_->get_grob_property ("grace"))
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
      
      if (to_boolean (i.elem_l_->get_grob_property ("grace")))
	grace_slur_endings_.push (i.elem_l_);
   }
}

void
Rhythmic_column_engraver::stop_translation_timestep ()
{
  if (ncol_p_) 
    {
      typeset_grob (ncol_p_);
      ncol_p_ =0;
    }
}

void
Rhythmic_column_engraver::start_translation_timestep ()
{
  grace_slur_endings_.clear ();
  dotcol_l_ =0;
  stem_l_ =0;
}

ADD_THIS_TRANSLATOR (Rhythmic_column_engraver);

