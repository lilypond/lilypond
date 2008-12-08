/*
  dot-column-engraver.cc -- implement Dot_column_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "rhythmic-head.hh"
#include "dot-column.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "translator.icc"
#include "item.hh"

class Dot_column_engraver : public Engraver
{
  Grob *dotcol_;
public:
  TRANSLATOR_DECLARATIONS (Dot_column_engraver);

protected:

  DECLARE_ACKNOWLEDGER (rhythmic_head);

  void stop_translation_timestep ();
};

Dot_column_engraver::Dot_column_engraver ()
{
  dotcol_ = 0;
}

void
Dot_column_engraver::stop_translation_timestep ()
{
  dotcol_ = 0;
}

void
Dot_column_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  Grob *d = unsmob_grob (info.grob ()->get_object ("dot"));
  if (d)
    {
      if (!dotcol_)
	dotcol_ = make_item ("DotColumn", SCM_EOL);

      Dot_column::add_head (dotcol_, info.grob ());
    }
}


ADD_ACKNOWLEDGER (Dot_column_engraver, rhythmic_head);
ADD_TRANSLATOR (Dot_column_engraver,
		/* doc */
		"Engrave dots on dotted notes shifted to the right of the"
		" note.  If omitted, then dots appear on top of the notes.",

		/* create */
		"DotColumn ",

		/* read */
		"",

		/* write */
		""
		);
