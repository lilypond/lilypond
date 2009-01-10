/*
  collision-engraver.cc -- implement Collision_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "note-column.hh"
#include "note-collision.hh"
#include "axis-group-interface.hh"
#include "item.hh"

class Collision_engraver : public Engraver
{
  Item *col_;
  vector<Grob*> note_columns_;

protected:
  DECLARE_ACKNOWLEDGER (note_column);
  void process_acknowledged ();
  void stop_translation_timestep ();
public:
  TRANSLATOR_DECLARATIONS (Collision_engraver);
};

void
Collision_engraver::process_acknowledged ()
{
  if (col_ || note_columns_.size () < 2)
    return;
  if (!col_)
    col_ = make_item ("NoteCollision", SCM_EOL);

  for (vsize i = 0; i < note_columns_.size (); i++)
    Note_collision_interface::add_column (col_, note_columns_[i]);
}

void
Collision_engraver::acknowledge_note_column (Grob_info i)
{
  if (Note_column::has_interface (i.grob ()))
    {
      /*should check Y axis? */
      if (Note_column::has_rests (i.grob ()) || i.grob ()->get_parent (X_AXIS))
	return;

      if (to_boolean (i.grob ()->get_property ("ignore-collision")))
	return;
      
      note_columns_.push_back (i.grob ());
    }
}

void
Collision_engraver::stop_translation_timestep ()
{
  col_ = 0;
  note_columns_.clear ();
}

Collision_engraver::Collision_engraver ()
{
  col_ = 0;
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Collision_engraver, note_column);

ADD_TRANSLATOR (Collision_engraver,
		/* doc */
		"Collect @code{NoteColumns}, and as soon as there are two or"
		" more, put them in a @code{NoteCollision} object.",

		/* create */
		"NoteCollision ",

		/* read */
		"",

		/* write */
		""
		);
