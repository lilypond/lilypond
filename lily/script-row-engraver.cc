/* 
  script-row-engraver.cc -- implement Script_row_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2006--2008 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "engraver.hh"

#include "accidental-placement.hh"
#include "item.hh"
#include "script-column.hh"
#include "side-position-interface.hh"

#include "translator.icc"


/**
   Find potentially colliding scripts, and put them in a
   Script_row
*/
class Script_row_engraver : public Engraver
{
  Grob *script_row_;
  vector<Grob*> scripts_;
  
public:
  TRANSLATOR_DECLARATIONS (Script_row_engraver);
protected:
  DECLARE_ACKNOWLEDGER (accidental_placement);
  DECLARE_ACKNOWLEDGER (side_position);
  void process_acknowledged ();
  void stop_translation_timestep ();
};

Script_row_engraver::Script_row_engraver ()
{
  script_row_ = 0;
}

void
Script_row_engraver::stop_translation_timestep ()
{
  if (script_row_)
    {
      for (vsize i = 0; i < scripts_.size (); i++)
	if (Accidental_placement::has_interface (scripts_[i])
	    || Side_position_interface::get_axis (scripts_[i]) == X_AXIS)
	  Script_column::add_side_positioned (script_row_, scripts_[i]);
    }

  scripts_.clear ();
  script_row_ = 0;
}

void
Script_row_engraver::acknowledge_side_position (Grob_info inf)
{
  Item *thing = dynamic_cast<Item *> (inf.grob ());
  if (thing)
    {
      if (!Item::is_non_musical (thing))
	scripts_.push_back (thing);
    }
}


void
Script_row_engraver::acknowledge_accidental_placement (Grob_info inf)
{
  scripts_.push_back (inf.grob ());
}


void
Script_row_engraver::process_acknowledged ()
{
  if (!script_row_ && scripts_.size () > 1)
    script_row_ = make_item ("ScriptRow", SCM_EOL);
}


ADD_ACKNOWLEDGER (Script_row_engraver, accidental_placement);
ADD_ACKNOWLEDGER (Script_row_engraver, side_position);
ADD_TRANSLATOR (Script_row_engraver,
		/* doc */
		"Determine order in horizontal side position elements.",

		/* create */
		"ScriptRow ",

		/* read */
		"",

		/* write */
		""
		);
