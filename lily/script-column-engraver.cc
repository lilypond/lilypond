/*   
  script-column-engraver.cc --  implement Script_column_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "engraver.hh"
#include "script-column.hh"
#include "item.hh"
#include "side-position-interface.hh"

/**
   Find potentially colliding scripts, and put them in a
   Script_column, that will fix the collisions.  */
class Script_column_engraver : public Engraver
{
  Grob *scol_;
  Link_array<Item> scripts_;

public:
  TRANSLATOR_DECLARATIONS(Script_column_engraver);
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void process_acknowledged_grobs ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
};


Script_column_engraver::Script_column_engraver ()
{
  scol_ =0;
}

void
Script_column_engraver::stop_translation_timestep ()
{
  if (scol_)
    {
      typeset_grob (scol_);
      scol_ =0;
    }
}

void
Script_column_engraver::start_translation_timestep ()
{
  scripts_.clear ();

}

void
Script_column_engraver::acknowledge_grob (Grob_info inf) 
{
  Item *thing = dynamic_cast<Item*> (inf.grob_);
  if (thing && Side_position_interface::has_interface (inf.grob_)) // ugh FIXME
    {
      if (!Item::breakable_b (thing)
	  && Side_position_interface::get_axis (inf.grob_) == Y_AXIS)
	{
	  scripts_.push (thing);
	}
    }
}

void
Script_column_engraver::process_acknowledged_grobs ()
{
  if (!scol_ && scripts_.size () > 1)
    {
      scol_ = new Item (get_property ("ScriptColumn"));
      announce_grob(scol_, SCM_EOL);
    }

  if (scol_)
    {
      for (int i=0; i < scripts_.size (); i++)
	Script_column::add_staff_sided (scol_, scripts_[i]);
      scripts_.clear ();
    }

}

ENTER_DESCRIPTION(Script_column_engraver,
/* descr */       "",
/* creats*/       "ScriptColumn",
/* accepts */     "general-music",
/* acks  */      "side-position-interface",
/* reads */       "",
/* write */       "");
