/* 
  note-spacing-engraver.cc -- implement Note_spacing_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2006--2007 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "engraver.hh"

#include "grob-array.hh"
#include "context.hh"
#include "item.hh"
#include "pointer-group-interface.hh"

#include "translator.icc"

class Note_spacing_engraver : public Engraver
{
  Grob *last_spacing_;
  Context *last_spacing_parent_context_;
  
  Grob *spacing_;

  void add_spacing_item (Grob *);
  TRANSLATOR_DECLARATIONS (Note_spacing_engraver);
protected:

  DECLARE_ACKNOWLEDGER (rhythmic_grob);
  DECLARE_ACKNOWLEDGER (note_column);
  void stop_translation_timestep ();
  virtual void finalize ();
  virtual void derived_mark () const;
};

void
Note_spacing_engraver::derived_mark () const
{
  if (last_spacing_parent_context_)
    scm_gc_mark (last_spacing_parent_context_->self_scm ());
}

Note_spacing_engraver::Note_spacing_engraver ()
{
  last_spacing_parent_context_ = 0;
  last_spacing_ = 0;
  spacing_ = 0;
}

void
Note_spacing_engraver::add_spacing_item (Grob *g)
{
  if (!spacing_)
    {
      spacing_ = make_item ("NoteSpacing", g->self_scm ());
    }
  
  if (spacing_)
    {
      Pointer_group_interface::add_grob (spacing_,
					 ly_symbol2scm ("left-items"),
					 g);

      if (last_spacing_)
	Pointer_group_interface::add_grob (last_spacing_,
					   ly_symbol2scm ("right-items"),
					   g);
    }
}


void
Note_spacing_engraver::acknowledge_note_column (Grob_info gi)
{
  add_spacing_item (gi.grob ());
}

void
Note_spacing_engraver::acknowledge_rhythmic_grob (Grob_info gi)
{
  add_spacing_item (gi.grob ());
}

void
Note_spacing_engraver::finalize ()
{
  if (last_spacing_
      && last_spacing_parent_context_
      && last_spacing_parent_context_ == context ()->get_parent_context ()
      && !unsmob_grob_array (last_spacing_->get_object ("right-items")))
    {
      SCM ccol = get_property ("currentCommandColumn");
      Grob *column = unsmob_grob (ccol);
      
      Pointer_group_interface::add_grob (last_spacing_,
					 ly_symbol2scm ("right-items"),
					 column);
    }
}

void
Note_spacing_engraver::stop_translation_timestep ()
{
  if (last_spacing_
      && last_spacing_parent_context_
      && last_spacing_parent_context_ == context ()->get_parent_context ())
    {
      Grob *sep = unsmob_grob (get_property ("currentCommandColumn"));
      if (sep)
	Pointer_group_interface::add_grob (last_spacing_,
					   ly_symbol2scm ("right-items"),
					   sep);
    }
  
  if (spacing_)
    {
      last_spacing_ = spacing_;
      last_spacing_parent_context_ = context ()->get_parent_context ();
      spacing_ = 0;
    }

}

ADD_ACKNOWLEDGER (Note_spacing_engraver, note_column);
ADD_ACKNOWLEDGER (Note_spacing_engraver, rhythmic_grob);

ADD_TRANSLATOR (Note_spacing_engraver,
		/* doc */ "Generates NoteSpacing, an object linking horizontal lines for use in spacing.",
		/* create */ "NoteSpacing",
		/* read */ "",
		/* write */ "");
