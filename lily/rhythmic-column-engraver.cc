/*
  rhythmic-column-grav.cc -- implement Rhythmic_column_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "slur.hh"
#include "engraver.hh"
#include "rhythmic-head.hh"
#include "stem.hh"
#include "note-column.hh"
#include "dot-column.hh"
#include "event.hh"
#include "item.hh"
#include "group-interface.hh"



/*
  this engraver  glues together stems, rests and note heads into a NoteColumn
  grob.

  It also generates spacing objects.  Originally, we have tried to
  have the spacing functionality at different levels.
  
  - by simply using the sequence of Separation-item as
  spacing-sequences (at staff level). Unfortunately, this fucks up if
  there are different kinds of tuplets in different voices (8th and
  8ths triplets combined made the program believe there were 1/12 th
  notes.).

  Doing it in a separate engraver using timing info is generally
  complicated (start/end time management), and fucks up if a voice
  changes staff.

  Now we do it from here again. This has the problem that voices can
  appear and disappear at will, leaving lots of loose ends (the note
  spacing engraver don't know where to connect the last note of the
  voice on the right with), but we don't complain about those, and let
  the default spacing do its work.

 */


class Rhythmic_column_engraver :public Engraver
{
  Link_array<Grob> rheads_;
  Grob * stem_;
  Grob * note_column_;
  Grob * dotcol_;

  Grob * last_spacing_;
  Grob * spacing_;
  
  TRANSLATOR_DECLARATIONS(Rhythmic_column_engraver);
protected:

  virtual void acknowledge_grob (Grob_info);
  virtual void process_acknowledged_grobs ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
};



Rhythmic_column_engraver::Rhythmic_column_engraver ()
{
  spacing_ =0 ;
  last_spacing_ = 0;
  
  stem_ =0;
  note_column_=0;
  dotcol_ =0;
}


void
Rhythmic_column_engraver::process_acknowledged_grobs ()
{
  if (rheads_.size ())
    {
      if (!note_column_)
	{
	  note_column_ = new Item (get_property ("NoteColumn"));
	  announce_grob(note_column_, SCM_EOL);


         spacing_ = new Item (get_property ("NoteSpacing"));
         spacing_->set_grob_property ("left-items", gh_cons (note_column_->self_scm (), SCM_EOL));

	 /*
	   Should insert a cause. Collision warnings go into the  void.
	   
	  */
         announce_grob(spacing_, SCM_EOL);

         if (last_spacing_)
           {
	     Pointer_group_interface::add_grob (last_spacing_,
						ly_symbol2scm ("right-items" ),
						note_column_);
           }

	}

      for (int i=0; i < rheads_.size (); i++)
	{
	  if (!rheads_[i]->get_parent (X_AXIS))
	    Note_column::add_head (note_column_, rheads_[i]);
	}
      rheads_.set_size (0);
    }

  
  if (note_column_)
    {
      if (dotcol_
	  && !dotcol_->get_parent (X_AXIS))
	{
	  Note_column::set_dotcol (note_column_, dotcol_);
	}

      if (stem_
	  && !stem_->get_parent (X_AXIS))
	{
	  Note_column::set_stem (note_column_, stem_);
	  stem_ = 0;
	}

    }
}

void
Rhythmic_column_engraver::acknowledge_grob (Grob_info i)
{
  Item * item =  dynamic_cast <Item *> (i.grob_);
  if (!item || item->get_parent (X_AXIS))
    return ; 
  if (Stem::has_interface (item))
    {
      stem_ = item;
    }
  else if (Rhythmic_head::has_interface (item))
    {
      rheads_.push (item);
    }
  else if (Dot_column::has_interface (item))
    {
      dotcol_ = item;
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
  dotcol_ =0;
  stem_ =0;
}



ENTER_DESCRIPTION(Rhythmic_column_engraver,
/* descr */       "Generates NoteColumn, an objects that groups stems, noteheads and rests.",
/* creats*/       "NoteColumn NoteSpacing",
/* accepts */     "",
/* acks  */      "stem-interface rhythmic-head-interface dot-column-interface",
/* reads */       "",
/* write */       "");
