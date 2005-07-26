/*
  script-column-engraver.cc -- implement Script_column_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1999--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "engraver.hh"
#include "script-column.hh"
#include "side-position-interface.hh"

#include "translator.icc"


/**
   Find potentially colliding scripts, and put them in a
   Script_column, that will fix the collisions.  */
class Script_column_engraver : public Engraver
{
  Grob *scol_;
  Link_array<Item> scripts_;

public:
  TRANSLATOR_DECLARATIONS (Script_column_engraver);
protected:
  DECLARE_ACKNOWLEDGER (side_position);
  void process_acknowledged ();
  void stop_translation_timestep ();
};

Script_column_engraver::Script_column_engraver ()
{
  scol_ = 0;
}

void
Script_column_engraver::stop_translation_timestep ()
{
  scol_ = 0;
  scripts_.clear ();
}

void
Script_column_engraver::acknowledge_side_position (Grob_info inf)
{
  Item *thing = dynamic_cast<Item *> (inf.grob ());
  if (thing)
    {
      if (!Item::is_breakable (thing)
	  && Side_position_interface::get_axis (inf.grob ()) == Y_AXIS)
	{
	  scripts_.push (thing);
	}
    }
}

void
Script_column_engraver::process_acknowledged ()
{
  if (!scol_ && scripts_.size () > 1)
    {
      scol_ = make_item ("ScriptColumn", SCM_EOL);
    }

  if (scol_)
    {
      for (int i = 0; i < scripts_.size (); i++)
	Script_column::add_staff_sided (scol_, scripts_[i]);
      scripts_.clear ();
    }
}
ADD_ACKNOWLEDGER (Script_column_engraver, side_position);
ADD_TRANSLATOR (Script_column_engraver,
		/* descr */ "",
		/* creats*/ "ScriptColumn",
		/* accepts */ "",
		/* reads */ "",
		/* write */ "");
