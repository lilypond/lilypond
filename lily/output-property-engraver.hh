/*   
  output-property-engraver.cc --  implement Output_property_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "output-property.hh"
#include "engraver.hh"

class Output_property_engraver : public Engraver
{
public:
  Output_property_engraver();
  VIRTUAL_COPY_CONS(Translator);
protected:
  
  Link_array<Output_property> props_;

  virtual void do_acknowledge_element (Score_element_info);
  virtual bool do_try_music (Music*);
};


Output_property_engraver::do_try_music (Music* m)
{
  if (Output_property * o = dynamic_cast<Output_property*> (m))
    {
      props_.push (m);
      return true;
    }
  return false;
}

void
Output_property_engraver::do_acknowledge_element (Score_element_info i)
{
  for (int i=props_.size (); i--; )
    {
      Output_property * o = props_[i];
      SCM pred = gh_car (o->pred_sym_val_list_);
      /*
	should typecheck pred. 
       */
      SCM result=gh_apply (pred,
			   gh_listify (i.elem_l_->self_scm_, SCM_UNDEFINED));
      if (to_boolean (result))
	{
	  i.elem_l_->set_elt_property (gh_cadr (o->pred_sym_val_list_),
				       gh_caddr (o->pred_sym_val_list_));
	}
    }
}

void
Output_property_engraver::do_pre_move_processing ()
{
  props_.clear ();
}
