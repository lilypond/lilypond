/*   
  axis-group-interface.cc --  implement Axis_group_interface
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "axis-group-interface.hh"
#include "score-element.hh"
#include "dimension-cache.hh"

Axis_group_interface::Axis_group_interface (Score_element*s)
{
  elt_l_ = s;
}

Axis_group_interface
Axis_group_interface (Score_element*s)
{
  return Axis_group_interface (s);
}

void
Axis_group_interface::add_element (Score_element *e)
{
  for (SCM ax = elt_l_->get_elt_property ("axes"); ax != SCM_EOL ; ax = gh_cdr (ax))
    {
      Axis a = (Axis) gh_scm2int (gh_car (ax));
      
      if (!e->parent_l (a))
	e->set_parent (elt_l_, a);
    }

  Pointer_group_interface (elt_l_).add_element (e);
  elt_l_->add_dependency (e);
}


bool
Axis_group_interface::axis_b (Axis a )const
{
  return elt_l_->has_extent_callback_b (group_extent_callback, a);
}

Interval
Axis_group_interface::relative_group_extent (Axis a, Score_element *common, SCM elts)
{
  Interval r;
  for (SCM s = elts; gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * se = unsmob_element (gh_car (s));
      Interval dims = se->extent (a);
      if (!dims.empty_b ())
	r.unite (dims + se->relative_coordinate (common, a));
    }
  return r;
}

Interval
Axis_group_interface::group_extent_callback (Score_element *me, Axis a)
{
  Score_element * common =(Score_element*) me;

  for (SCM s = me->get_elt_pointer ("elements"); gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * se = unsmob_element (gh_car (s));
      common = se->common_refpoint (common, a);
    }

  Real my_coord = me->relative_coordinate (common, a);
  Interval r (relative_group_extent (a, common, me->get_elt_pointer ("elements")));

  return r - my_coord;
}

void
Axis_group_interface::set_interface ()
{
  if (!has_interface_b ())
    {
      elt_l_->set_elt_pointer ("elements", SCM_EOL);


      Group_interface (elt_l_, "interfaces").add_thing (ly_symbol2scm ("Axis_group"));
    }
}

void
Axis_group_interface::set_axes (Axis a1, Axis a2)
{
  // set_interface () ?
  SCM sa1= gh_int2scm (a1);
  SCM sa2 = gh_int2scm (a2);

  SCM prop = elt_l_->get_elt_property ("axes");
  
  if (prop == SCM_UNDEFINED
      || scm_memq (sa1, prop) == SCM_BOOL_F
      || scm_memq (sa2, prop) == SCM_BOOL_F)
    {
      SCM ax = gh_cons (sa1, SCM_EOL);
      if (a1 != a2)
	ax= gh_cons (sa2, ax);
      elt_l_->set_elt_property ("axes", ax);
    }

  if (a1 != X_AXIS && a2 != X_AXIS)
    elt_l_->set_extent_callback (0, X_AXIS);
  if (a1 != Y_AXIS && a2 != Y_AXIS)
    elt_l_->set_extent_callback (0, Y_AXIS);
  
  elt_l_->set_extent_callback (Axis_group_interface::group_extent_callback,a1);
  elt_l_->set_extent_callback (Axis_group_interface::group_extent_callback,a2);
}

Link_array<Score_element> 
Axis_group_interface::get_children ()
{
  Link_array<Score_element> childs;
  childs.push (elt_l_) ;

  if (!has_interface_b ())
    return childs;
  
  for (SCM ep = elt_l_->get_elt_pointer ("elements"); gh_pair_p (ep); ep = gh_cdr (ep))
    {
      Score_element* e = unsmob_element (gh_car (ep));
      if (e)
	childs.concat (Axis_group_interface (e).get_children ());
    }
  
  return childs;
}

bool
Axis_group_interface::has_interface_b ()
{
  SCM ifs = elt_l_->get_elt_property ("interfaces");

  if (!gh_pair_p (ifs ))
    return false;
  return scm_memq (ly_symbol2scm ("Axis_group"),ifs)  != SCM_BOOL_F;
}


