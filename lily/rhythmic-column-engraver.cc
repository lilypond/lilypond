/*
  rhythmic-column-engraver.cc -- implement Rhythmic_column_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "rhythmic-head.hh"
#include "stem.hh"
#include "note-column.hh"
#include "item.hh"
#include "dot-column.hh"
#include "pointer-group-interface.hh"

#include "translator.icc"

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

class Rhythmic_column_engraver : public Engraver
{
  vector<Grob*> rheads_;
  Grob *stem_;
  Grob *note_column_;
  Grob *dotcol_;
  Grob *arpeggio_;

  TRANSLATOR_DECLARATIONS (Rhythmic_column_engraver);
protected:

  DECLARE_ACKNOWLEDGER (stem);
  DECLARE_ACKNOWLEDGER (rhythmic_head);
  DECLARE_ACKNOWLEDGER (arpeggio);
  void process_acknowledged ();
  void stop_translation_timestep ();
};

Rhythmic_column_engraver::Rhythmic_column_engraver ()
{

  stem_ = 0;
  note_column_ = 0;
  arpeggio_ = 0;
}


void
Rhythmic_column_engraver::process_acknowledged ()
{
  if (rheads_.size ())
    {
      if (!note_column_)
	note_column_ = make_item ("NoteColumn", rheads_[0]->self_scm ());

      for (vsize i = 0; i < rheads_.size (); i++)
	if (!rheads_[i]->get_parent (X_AXIS))
	  Note_column::add_head (note_column_, rheads_[i]);

      rheads_.resize (0);
    }

  if (note_column_)
    {
      if (stem_
	  && !stem_->get_parent (X_AXIS))
	{
	  Note_column::set_stem (note_column_, stem_);
	  stem_ = 0;
	}

      if (arpeggio_)
	note_column_->set_object ("arpeggio", arpeggio_->self_scm ());
    }
}

void
Rhythmic_column_engraver::acknowledge_stem (Grob_info i)
{
  stem_ = i.grob ();
}

void
Rhythmic_column_engraver::acknowledge_rhythmic_head (Grob_info i)
{
  rheads_.push_back (i.grob ());
}

void
Rhythmic_column_engraver::acknowledge_arpeggio (Grob_info i)
{
  arpeggio_ = i.grob ();
}

void
Rhythmic_column_engraver::stop_translation_timestep ()
{
  note_column_ = 0;
  stem_ = 0;
  arpeggio_ = 0;
}

ADD_ACKNOWLEDGER (Rhythmic_column_engraver, stem);
ADD_ACKNOWLEDGER (Rhythmic_column_engraver, rhythmic_head);
ADD_ACKNOWLEDGER (Rhythmic_column_engraver, arpeggio);

ADD_TRANSLATOR (Rhythmic_column_engraver,
		/* doc */
		"Generate @code{NoteColumn}, an object that groups stems,"
		" note heads, and rests.",

		/* create */
		"NoteColumn ",

		/* read */
		"",

		/* write */
		""
		);
