/*   
  spaceable-grob.cc --  implement Spaceable_grob
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include <stdio.h> 
#include <math.h>

#include "spaceable-grob.hh"
#include "grob.hh"
#include "warn.hh"
#include "spring.hh"
#include "group-interface.hh"

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
  //  printf ("rod %lf\n", d);

  
  SCM mins = get_minimum_distances (me);
  SCM newdist = gh_double2scm (d);
  for (SCM s = mins; gh_pair_p (s); s = ly_cdr (s))
    {
      SCM dist = ly_car (s);
      if (ly_car (dist) == p->self_scm ())
	{
	  gh_set_cdr_x (dist, scm_max (ly_cdr (dist),
				       newdist));
	  return ;
	}
    }

  mins = gh_cons (gh_cons (p->self_scm (), newdist), mins);
  me->set_grob_property ("minimum-distances", mins);
}

void
Spaceable_grob::add_spring (Grob*me, Grob * p, Real d, Real strength, bool expand_only)
{
  //  printf ("dist %lf, str %lf\n", d, strength); 
  if (d <= 0.0 || strength <= 0.0)
    {
      programming_error ("Adding reverse spring! Setting to unit spring");
      d = 1.0;
      strength = 1.0;
    }
  
  if (isinf (d) || isnan(d)
      || isnan (strength))
    {
      /*
	strength == INF is possible. It means fixed distance.
       */
      programming_error ("Insane distance found.");
      d = 1.0;
      strength = 1.0;
    }
    
#ifndef NDEBUG
  SCM mins = me->get_grob_property ("ideal-distances");
  for (SCM s = mins; gh_pair_p (s); s = ly_cdr (s))
    {
      Spring_smob * sp = unsmob_spring(ly_car (s));
      if (sp->other_ == p)
	{
	  programming_error ("already have that spring");
	  return ;
	}
    }
#endif
  
  Spring_smob spring;
  spring.strength_f_ = strength;
  spring.distance_f_ = d;
  spring.expand_only_b_ = expand_only;
  spring.other_ = p;
  
  Group_interface::add_thing (me, ly_symbol2scm ("ideal-distances"), spring.smobbed_copy ());
}


void
Spaceable_grob::remove_interface (Grob*me)
{
  me->remove_grob_property ("minimum-distances");
  me->remove_grob_property ("spacing-wishes");
  me->remove_grob_property ("ideal-distances");
}



ADD_INTERFACE (Spaceable_grob,"spaceable-grob-interface",
  "A grob (a Paper_column) that takes part in the
spacing problem. ",
  "measure-length spacing-wishes penalty minimum-distances ideal-distances
left-neighbors right-neighbors");

