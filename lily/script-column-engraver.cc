/*   
  g-script-column-engraver.cc --  implement Script_column_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  Grob *scol_p_;
  Link_array<Item> script_l_arr_;

public:
  Script_column_engraver ();
  VIRTUAL_COPY_CONS(Translator);
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();
  virtual void  stop_translation_timestep ();
  virtual void  start_translation_timestep ();
};


Script_column_engraver::Script_column_engraver()
{
  scol_p_ =0;
}

void
Script_column_engraver::stop_translation_timestep ()
{
  if (scol_p_)
    {
      typeset_grob (scol_p_);
      scol_p_ =0;
    }
}

void
Script_column_engraver::start_translation_timestep ()
{
  script_l_arr_.clear ();

}

void
Script_column_engraver::acknowledge_grob(Grob_info inf) 
{
  Item *thing = dynamic_cast<Item*> (inf.elem_l_);
  if (thing && Side_position::has_interface (inf.elem_l_)) // ugh FIXME
    {
      if (!Item::breakable_b (thing)
	  && Side_position::get_axis (inf.elem_l_) == Y_AXIS)
	{
	  script_l_arr_.push (thing);
	}
    }
}

void
Script_column_engraver::create_grobs ()
{
  if (!scol_p_ && script_l_arr_.size () > 1)
    {
      scol_p_ = new Item (get_property ("ScriptColumn"));


      announce_grob (scol_p_, 0);
    }

  if (scol_p_)
    {
      for (int i=0; i < script_l_arr_.size (); i++)
	Script_column::add_staff_sided (scol_p_, script_l_arr_[i]);
      script_l_arr_.clear ();
    }
}
ADD_THIS_TRANSLATOR(Script_column_engraver);
