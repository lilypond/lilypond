/*   
  spaceable-element.cc --  implement Spaceable_element
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "spaceable-element.hh"
#include "score-element.hh"
#include "warn.hh"

SCM
Spaceable_element::get_minimum_distances ( Score_element*me)
{
  return me->get_elt_property ("minimum-distances");
}

void
Spaceable_element::add_rod (Score_element *me , Score_element * p, Real d)
{
  SCM mins = get_minimum_distances (me);
  SCM newdist= 		       gh_double2scm (d);
  for (; gh_pair_p (mins); mins = gh_cdr (mins))
    {
      SCM dist = gh_car (mins);
      if (gh_car (dist) == p->self_scm_)
	{
	  gh_set_cdr_x (dist, scm_max (gh_cdr (dist),
				       newdist));
	  return ;
	}
    }

  mins = gh_cons (gh_cons (p->self_scm_, newdist), mins);
  me->set_elt_property ("minimum-distances", mins);
}

SCM
Spaceable_element::get_ideal_distances (Score_element*me)
{
  return me->get_elt_property ("ideal-distances");
}

void
Spaceable_element::add_spring (Score_element*me, Score_element * p, Real d, Real s)
{
  SCM mins = get_ideal_distances (me);
  SCM newdist= gh_double2scm (d);
  for (; gh_pair_p (mins); mins = gh_cdr (mins))
    {
      SCM dist = gh_car (mins);
      if (gh_car (dist) == p->self_scm_)
	{
	  programming_error("already have that spring");
	  /*	  gh_set_car_x (gh_cdr (dist), scm_max (gh_cadr (dist),
		  newdist));*/
	  return ;
	}
    }
  SCM newstrength= gh_double2scm (s);  
  
  mins = gh_cons (gh_cons (p->self_scm_, gh_cons (newdist, newstrength)), mins);
  me->set_elt_property ("ideal-distances", mins);
}


void
Spaceable_element::remove_interface (Score_element*me)
{
  me->remove_elt_property ("minimum-distances");
  me->remove_elt_property ("ideal-distances");
  me->remove_elt_property ("dir-list");
}


void
Spaceable_element::set_interface (Score_element*me)
{
  me->set_elt_property ("minimum-distances", SCM_EOL);
  me->set_elt_property ("ideal-distances", SCM_EOL);
  me->set_elt_property ("dir-list",SCM_EOL) ;  
}
