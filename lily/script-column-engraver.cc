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
  Score_element *scol_p_;
  Link_array<Item> script_l_arr_;

public:
  Script_column_engraver ();
  VIRTUAL_COPY_CONS(Translator);
protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void  do_pre_move_processing ();
  virtual void  do_post_move_processing ();
};


Script_column_engraver::Script_column_engraver()
{
  scol_p_ =0;
}

void
Script_column_engraver::do_pre_move_processing ()
{
  if (scol_p_)
    {
      typeset_element (scol_p_);
      scol_p_ =0;
    }
}

void
Script_column_engraver::do_post_move_processing ()
{
  script_l_arr_.clear ();

}

void
Script_column_engraver::acknowledge_element( Score_element_info inf) 
{
  if (Side_position::has_interface (inf.elem_l_)) // ugh FIXME
    {
      Item *thing = dynamic_cast<Item*> (inf.elem_l_);
      if (thing
	  && !Item::breakable_b (thing)
	  && Side_position::get_axis (inf.elem_l_) == Y_AXIS)
	{
	  script_l_arr_.push (thing);
	}
    }
}

void
Script_column_engraver::process_acknowledged ()
{
  if (!scol_p_ && script_l_arr_.size () > 1)
    {
      scol_p_ = new Item (get_property ("basicScriptColumnProperties"));
      scol_p_->set_elt_property ("scripts", SCM_EOL);  

      announce_element (Score_element_info (scol_p_, 0));
    }

  if (scol_p_)
    {
      for (int i=0; i < script_l_arr_.size (); i++)
	Script_column::add_staff_sided (scol_p_, script_l_arr_[i]);
      script_l_arr_.clear ();
    }
}
ADD_THIS_TRANSLATOR(Script_column_engraver);
