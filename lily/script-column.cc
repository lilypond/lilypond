/*   
  script-column.cc --  implement Script_column
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "script-column.hh"
#include "side-position-interface.hh"
#include "warn.hh"
#include "group-interface.hh"

void
Script_column::add_staff_sided (Grob *me, Item *i)
{
  SCM p = i->get_grob_property ("script-priority");
  if (!gh_number_p (p))
    return;

  Pointer_group_interface::add_grob (me, ly_symbol2scm ("scripts"),i);
  
  me->add_dependency (i);
}

LY_DEFINE(grob_script_priority_less,
	  "ly:grob-script-priority-less", 2, 0, 0, 
	  (SCM a, SCM b),
	  "Compare two grobs by script priority. For internal use.")
{
  Grob * i1 = unsmob_grob (a);
  Grob* i2 = unsmob_grob (b);

  SCM p1 = i1->get_grob_property ("script-priority");
  SCM p2 = i2->get_grob_property ("script-priority");

  return gh_scm2int (p1) < gh_scm2int (p2) ? SCM_BOOL_T : SCM_BOOL_F;
}



MAKE_SCHEME_CALLBACK (Script_column,before_line_breaking,1);
SCM
Script_column::before_line_breaking (SCM smob)
{
  Grob* me = unsmob_grob (smob);
  Drul_array<SCM> scripts (SCM_EOL, SCM_EOL);
  Link_array<Grob> staff_sided 
    = Pointer_group_interface__extract_grobs (me, (Grob*)0, "scripts");
				     
  for (int i=0; i < staff_sided.size (); i++)
    {
      Grob* g = staff_sided[i];
      Direction d = Side_position_interface::get_direction (g);
      if (!d)
	{
	  programming_error ( "No direction for script?");
	  d = DOWN;
	  g->set_grob_property ("direction", gh_int2scm (d));
	}
      
      scripts[d] = scm_cons (g->self_scm(), scripts[d]);
    }

  Direction d = DOWN;
  do {
    SCM ss = scm_reverse_x (scripts[d], SCM_EOL);
    
    ss = scm_stable_sort_x (ss,  grob_script_priority_less_proc);

    Grob * last = 0;
    for (SCM s = ss; gh_pair_p (s); s = gh_cdr (s))
      {
	Grob* g = unsmob_grob (gh_car (s));
	if (last)
	  Side_position_interface::add_support (g,last);

	last = g;
      }
    
  } while (flip (&d) != DOWN);

  return SCM_UNSPECIFIED;
}


ADD_INTERFACE (Script_column,"script-column-interface",
  "An interface that sorts scripts according to their @code{script-priority}",
  "");


