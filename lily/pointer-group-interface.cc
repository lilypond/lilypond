/*   
  group-interface.cc --  implement Pointer_group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "group-interface.hh"
#include "score-element.hh"

Pointer_group_interface::Pointer_group_interface (Score_element const* e)
{
  elt_l_ = (Score_element*)e;
  name_ = "elements";
}


Pointer_group_interface::Pointer_group_interface (Score_element const *e, String s)
{
  elt_l_ =(Score_element*)e;
  name_ = s;
} 
bool
Pointer_group_interface::has_interface () 
{
  SCM el = elt_l_->get_elt_property (name_.ch_C());

  return el == SCM_EOL || gh_pair_p (el);
}


void
Pointer_group_interface::add_element (Score_element*p) 
{
  elt_l_->set_elt_property (name_.ch_C(),
			   gh_cons (p->self_scm_,
				    elt_l_->get_elt_property (name_.ch_C())));
}

int
Pointer_group_interface::count ()
{
  return scm_ilength (elt_l_->get_elt_property (name_.ch_C()));
}

void
Pointer_group_interface::set_interface ()
{
  if (!has_interface ())
    {
      elt_l_->set_elt_property (name_.ch_C(), SCM_EOL);
    }
} 

