/*
  grace-spacing-engraver.cc -- implement Grace_spacing_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2006--2009 Han-Wen <hanwen@lilypond.org>

*/

#include "engraver.hh"
#include "moment.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"

#include "translator.icc"

class Grace_spacing_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Grace_spacing_engraver);

protected:

  Moment last_moment_;
  Spanner *grace_spacing_;
  
  void process_music ();
  void stop_translation_timestep ();
};


Grace_spacing_engraver::Grace_spacing_engraver ()
{
  grace_spacing_ = 0;
}

void
Grace_spacing_engraver::process_music ()
{
  Moment now = now_mom ();
  if (!last_moment_.grace_part_ and now.grace_part_)
    {
      grace_spacing_ = make_spanner ("GraceSpacing", SCM_EOL);
    }


  if (grace_spacing_ && (now.grace_part_ || last_moment_.grace_part_))
    {
      Grob *column = unsmob_grob (get_property ("currentMusicalColumn"));
      Pointer_group_interface::add_grob (grace_spacing_,
					 ly_symbol2scm ("columns"),
					 column);

      column->set_object ("grace-spacing", grace_spacing_->self_scm ());

      if (!grace_spacing_->get_bound (LEFT))
	grace_spacing_->set_bound (LEFT, column);
      else
	grace_spacing_->set_bound (RIGHT, column);
    }
}

void
Grace_spacing_engraver::stop_translation_timestep ()
{
  last_moment_ = now_mom ();

  if (!last_moment_.grace_part_)
    grace_spacing_ = 0;
}


ADD_TRANSLATOR (Grace_spacing_engraver,
		"Bookkeeping of shortest starting and playing notes in grace"
		" note runs.",

		/* create */
		"GraceSpacing ",

		/* read */
		"currentMusicalColumn ",
		
		/* write */
		""
		);
