/*
  script-column-engraver.cc -- implement Script_column_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "script-column.hh"
#include "side-position-interface.hh"
#include "item.hh"

#include "translator.icc"

/**
   Find potentially colliding scripts, and put them in a
   Script_column, that will fix the collisions.  */
class Script_column_engraver : public Engraver
{
  Grob *script_column_;
  vector<Grob*> scripts_;

public:
  TRANSLATOR_DECLARATIONS (Script_column_engraver);
protected:
  DECLARE_ACKNOWLEDGER (side_position);
  void process_acknowledged ();
  void stop_translation_timestep ();
};

Script_column_engraver::Script_column_engraver ()
{
  script_column_ = 0;
}

void
Script_column_engraver::stop_translation_timestep ()
{
  if (script_column_)
    {
      for (vsize i = 0; i < scripts_.size (); i++)
	if (Side_position_interface::get_axis (scripts_[i]) == Y_AXIS)
	  Script_column::add_side_positioned (script_column_, scripts_[i]);
    }

  script_column_ = 0;
  scripts_.clear ();
}

void
Script_column_engraver::acknowledge_side_position (Grob_info inf)
{
  Item *thing = dynamic_cast<Item *> (inf.grob ());
  if (thing)
    {
      if (!Item::is_non_musical (thing))
	scripts_.push_back (thing);
    }
}

void
Script_column_engraver::process_acknowledged ()
{
  if (!script_column_ && scripts_.size () > 1)
    script_column_ = make_item ("ScriptColumn", SCM_EOL);
}

ADD_ACKNOWLEDGER (Script_column_engraver, side_position);
ADD_TRANSLATOR (Script_column_engraver,
		/* doc */
		"Find potentially colliding scripts and put them into a"
		" @code{ScriptColumn} object; that will fix the collisions.",

		/* create */
		"ScriptColumn ",

		/* read */
		"",

		/* write */
		""
		);
