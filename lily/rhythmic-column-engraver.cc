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
  Grob * stem_l_;
  Grob * note_column_;
  Grob * dotcol_l_;

  Grob * last_spacing_;
  Grob * spacing_;
  
  TRANSLATOR_DECLARATIONS(Rhythmic_column_engraver);
protected:

  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
};



Rhythmic_column_engraver::Rhythmic_column_engraver ()
{
  spacing_ =0 ;
  last_spacing_ = 0;
  
  stem_l_ =0;
  note_column_=0;
  dotcol_l_ =0;
}


void
Rhythmic_column_engraver::create_grobs ()
{
  if (rhead_l_arr_.size ())
    {
      if (!note_column_)
	{
	  note_column_ = new Item (get_property ("NoteColumn"));
	  Note_column::set_interface (note_column_);
	  announce_grob (note_column_, 0);

	  spacing_ = new Item (get_property ("NoteSpacing"));
	  spacing_->set_grob_property ("left-item", note_column_->self_scm ());
	  announce_grob (spacing_, 0);

	  if (last_spacing_)
	    {
	      last_spacing_->set_grob_property ("right-item" , note_column_->self_scm ());
	    }
	}

      for (int i=0; i < rhead_l_arr_.size (); i++)
	{
	  if (!rhead_l_arr_[i]->get_parent (X_AXIS))
	    Note_column::add_head (note_column_, rhead_l_arr_[i]);
	}
      rhead_l_arr_.set_size (0);
    }

  
  if (note_column_)
    {
      if (dotcol_l_
	  && !dotcol_l_->get_parent (X_AXIS))
	{
	  Note_column::set_dotcol (note_column_, dotcol_l_);
	}

      if (stem_l_
	  && !stem_l_->get_parent (X_AXIS))
	{
	  Note_column::set_stem (note_column_, stem_l_);
	  stem_l_ = 0;
	}

    }
}

void
Rhythmic_column_engraver::acknowledge_grob (Grob_info i)
{
  Item * item =  dynamic_cast <Item *> (i.grob_l_);
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
}

void
Rhythmic_column_engraver::stop_translation_timestep ()
{
  if (note_column_) 
    {
      typeset_grob (note_column_);
      note_column_ =0;
    }

  if (spacing_)
    {
      typeset_grob (spacing_);
      last_spacing_ = spacing_;
      spacing_ =0;
    }
}

void
Rhythmic_column_engraver::start_translation_timestep ()
{
  dotcol_l_ =0;
  stem_l_ =0;
}



ENTER_DESCRIPTION(Rhythmic_column_engraver,
/* descr */       "Generates NoteColumn, an objects that groups stems, noteheads and rests.",
/* creats*/       "NoteColumn",
/* acks  */       "stem-interface rhythmic-head-interface dot-column-interface",
/* reads */       "",
/* write */       "");
