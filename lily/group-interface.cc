/*   
  group-interface.cc --  implement Group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "group-interface.hh"
#include "score-element.hh"

Group_interface::Group_interface (Score_element const* e)
{
  elt_l_ = (Score_element*)e;
  name_ = "elements";
}


Group_interface::Group_interface (Score_element const *e, String s)
{
  elt_l_ =(Score_element*)e;
  name_ = s;
} 
bool
Group_interface::has_interface_b () 
{
  SCM el = elt_l_->get_elt_property (name_);

  return el == SCM_EOL || gh_pair_p (el);
}


void
Group_interface::add_element (Score_element*p) 
{
  p->used_b_ =  true;
  elt_l_->used_b_ = true;
  
  elt_l_->set_elt_property (name_,
			    gh_cons (p->self_scm_, elt_l_->get_elt_property (name_)));
}

int
Group_interface::count ()
{
  return scm_ilength (elt_l_->get_elt_property (name_));
}

void
Group_interface::set_interface ()
{
  if (!has_interface_b ())
    {
      elt_l_->set_elt_property (name_, SCM_EOL);
    }
}

Group_interface
group (Score_element*s)
{
  Group_interface gi (s);
  return gi;
}
