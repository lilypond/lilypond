/* 
  note-spacing-engraver.cc -- implement Note_spacing_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2006--2009 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "engraver.hh"

#include "grob-array.hh"
#include "context.hh"
#include "item.hh"
#include "pointer-group-interface.hh"

#include <map>

#include "translator.icc"

class Note_spacing_engraver : public Engraver
{
  typedef map <Context*, Grob*> Last_spacing_map;
  Last_spacing_map last_spacings_;
  Grob *last_spacing_;
  
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
  for (Last_spacing_map::const_iterator i = last_spacings_.begin ();
       i != last_spacings_.end (); i++)
    scm_gc_mark (i->first->self_scm ());
}

Note_spacing_engraver::Note_spacing_engraver ()
{
  spacing_ = 0;
  last_spacing_ = 0;
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
  Context *parent = context ()->get_parent_context ();
  Grob *last_spacing = last_spacings_[parent];

  if (last_spacing
      && !unsmob_grob_array (last_spacing->get_object ("right-items")))
    {
      Grob *col  = unsmob_grob (get_property ("currentCommandColumn"));
      
      Pointer_group_interface::add_grob (last_spacing,
					 ly_symbol2scm ("right-items"),
					 col);
    }
}

void
Note_spacing_engraver::stop_translation_timestep ()
{
  Context *parent = context ()->get_parent_context ();
  Grob *last_spacing = last_spacings_[parent];

  if (last_spacing
      && to_boolean (get_property ("hasStaffSpacing")))
    {
      Grob *col = unsmob_grob (get_property ("currentCommandColumn"));
      Pointer_group_interface::add_grob (last_spacing,
					 ly_symbol2scm ("right-items"),
					 col);
    }
  
  if (spacing_)
    {
      last_spacings_[parent] = spacing_;
      last_spacing_ = spacing_;
      spacing_ = 0;
    }

}

ADD_ACKNOWLEDGER (Note_spacing_engraver, note_column);
ADD_ACKNOWLEDGER (Note_spacing_engraver, rhythmic_grob);

ADD_TRANSLATOR (Note_spacing_engraver,
		/* doc */
		"Generate @code{NoteSpacing}, an object linking horizontal"
		" lines for use in spacing.",

		/* create */
		"NoteSpacing ",

		/* read */
		"",

		/* write */
		""
		);
