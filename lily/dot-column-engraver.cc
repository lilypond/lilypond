/*
  dot-column-engraver.cc -- implement Dot_column_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "rhythmic-head.hh"
#include "dot-column.hh"
#include "side-position-interface.hh"
#include "engraver.hh"
#include "stem.hh"
#include "translator.icc"

class Dot_column_engraver : public Engraver
{
  Grob *dotcol_;
  Grob *stem_;
  Link_array__Item_ heads_;
public:
  TRANSLATOR_DECLARATIONS (Dot_column_engraver);

protected:

  DECLARE_ACKNOWLEDGER (stem);
  DECLARE_ACKNOWLEDGER (rhythmic_head);

  void stop_translation_timestep ();
};

Dot_column_engraver::Dot_column_engraver ()
{
  dotcol_ = 0;
  stem_ = 0;
}

void
Dot_column_engraver::stop_translation_timestep ()
{
  /*
    Add the stem to the support so dots stay clear of flags.

    See [Ross, p 171]
  */
  if (stem_ && dotcol_)
    dotcol_->set_object ("stem", stem_->self_scm ());

  dotcol_ = 0;
  heads_.clear ();
  stem_ = 0;
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

void
Dot_column_engraver::acknowledge_stem (Grob_info info)
{
  stem_ = info.grob ();
}

ADD_ACKNOWLEDGER (Dot_column_engraver, stem);
ADD_ACKNOWLEDGER (Dot_column_engraver, rhythmic_head);
ADD_TRANSLATOR (Dot_column_engraver,
		/* doc */ "Engraves dots on dotted notes shifted to the right of the note.\n"
		"If omitted, then dots appear on top of the notes.",
		/* create */ "DotColumn",
		/* accept */ "",
		/* read */ "",
		/* write */ "");
