/*   
  spaceable-grob.cc --  implement Spaceable_grob
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "spaceable-grob.hh"
#include "grob.hh"
#include "warn.hh"

SCM
Spaceable_grob::get_minimum_distances (Grob*me)
{
  return me->get_grob_property ("minimum-distances");
}

/*todo: merge code of spring & rod?
 */
void
Spaceable_grob::add_rod (Grob *me , Grob * p, Real d)
{
  SCM mins = get_minimum_distances (me);
  SCM newdist = gh_double2scm (d);
  for (SCM s = mins; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM dist = gh_car (s);
      if (gh_car (dist) == p->self_scm ())
	{
	  gh_set_cdr_x (dist, scm_max (gh_cdr (dist),
				       newdist));
	  return ;
	}
    }

  mins = gh_cons (gh_cons (p->self_scm (), newdist), mins);
  me->set_grob_property ("minimum-distances", mins);
}

void
Spaceable_grob::add_spring (Grob*me, Grob * p, Real d, Real strength)
{
  SCM mins = me->get_grob_property ("ideal-distances");
  
  
  SCM newdist= gh_double2scm (d);
  for (SCM s = mins; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM dist = gh_car (s);
      if (gh_car (dist) == p->self_scm ())
	{
	  programming_error ("already have that spring");
	  return ;
	}
    }
  SCM newstrength= gh_double2scm (strength);  
  
  mins = gh_cons (gh_cons (p->self_scm (), gh_cons (newdist, newstrength)), mins);
  me->set_grob_property ("ideal-distances", mins);
}


void
Spaceable_grob::remove_interface (Grob*me)
{
  me->remove_grob_property ("minimum-distances");
  me->remove_grob_property ("ideal-distances");
  me->remove_grob_property ("dir-list");
}


void
Spaceable_grob::set_interface (Grob*me)
{
  me->set_interface (ly_symbol2scm ("spaceable-grob-interface"));
}
