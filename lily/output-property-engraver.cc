/*   
  output-property-engraver.cc --  implement Output_property_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "output-property.hh"
#include "engraver.hh"
#include "score-element.hh"

class Output_property_engraver : public Engraver
{
public:
  VIRTUAL_COPY_CONS(Translator);
protected:
  
  Link_array<Output_property> props_;

  virtual void do_pre_move_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_music (Music*);
};


bool
Output_property_engraver::do_try_music (Music* m)
{
  if (Output_property * o = dynamic_cast<Output_property*> (m))
    {
      props_.push (o);
      return true;
    }
  return false;
}

void
Output_property_engraver::acknowledge_element (Score_element_info inf)
{
  for (int i=props_.size (); i--; )
    {
      Output_property * o = props_[i];
      SCM pred = o->get_mus_property ("predicate");
      
      /*
	should typecheck pred. 
       */
      SCM result=gh_apply (pred,
			   gh_list (inf.elem_l_->self_scm (), SCM_UNDEFINED));
      if (to_boolean (result))
	{
	  SCM sym = o->get_mus_property ("symbol");
	  SCM val = o->get_mus_property ("value");
	  inf.elem_l_->set_elt_property (sym, val);
	}
    }
}

void
Output_property_engraver::do_pre_move_processing ()
{
  props_.clear ();
}

ADD_THIS_TRANSLATOR(Output_property_engraver);
