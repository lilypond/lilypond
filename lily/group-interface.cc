/*   
  group-interface.cc --  implement Group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "group-interface.hh"
#include "score-element.hh"

/*
  ugh: clean me, junk elt_l_ field
 */
Group_interface::Group_interface (Score_element * e)
{
  elt_l_ = (Score_element*)e;
  name_ = "elements";
}


Group_interface::Group_interface (Score_element  *e, String s)
{
  elt_l_ =(Score_element*)e;
  name_ = s;
} 
bool
Group_interface::has_interface () 
{
  SCM el = elt_l_->get_elt_property (name_.ch_C());

  return el == SCM_EOL || gh_pair_p (el);
}



void
Group_interface::add_thing (SCM s)
{
  elt_l_->set_elt_property (name_.ch_C (),
 			    gh_cons (s, elt_l_->get_elt_property (name_.ch_C())));
}


int
Group_interface::count ()
{
  return scm_ilength (elt_l_->get_elt_property (name_.ch_C()));
}



