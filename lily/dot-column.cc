/*
  dot-column.cc -- implement Dot_column

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "dot-column.hh"

#include <cstdio>
#include <cmath>
#include <map>
using namespace std;

#include "dots.hh"
#include "dot-column.hh"
#include "rhythmic-head.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"
#include "side-position-interface.hh"
#include "axis-group-interface.hh"
#include "stem.hh"
#include "grob.hh"
#include "pointer-group-interface.hh"
#include "dot-configuration.hh"

/*
  TODO: let Dot_column communicate with stem via Note_column.
*/

MAKE_SCHEME_CALLBACK (Dot_column, side_position, 1);
SCM
Dot_column::side_position (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  extract_grob_set (me, "dots", dots);
  
  for (vsize i = 0; i  < dots.size (); i++)
    {
      Grob *head = dots[i]->get_parent (Y_AXIS);
      Grob *stem = head ? unsmob_grob (head->get_object ("stem")) : 0;
      if (stem
	  && !Stem::get_beam (stem)
	  && Stem::duration_log (stem) > 2
	  && !Stem::is_invisible (stem))
	{
	  /*
	    trigger stem end & direction calculation.
	    
	    This will add the stem to the support if a flag collision happens.
	  */
	  stem->get_property ("stem-end-position");
	}
    }
  
  return Side_position_interface::x_aligned_side (smob, SCM_EOL);
}

MAKE_SCHEME_CALLBACK (Dot_column, calc_positioning_done, 1);
SCM
Dot_column::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);  

  me->set_property ("positioning-done", SCM_BOOL_T);

  vector<Grob*> dots
    = extract_grob_array (me, "dots");

  { /*
      Trigger note collision resolution first, since that may kill off
      dots when merging.
    */
    Grob *c = 0;
    for (vsize i = dots.size (); i--;)
      {
	Grob *n = dots[i]->get_parent (Y_AXIS);
	if (c)
	  c = n->common_refpoint (c, X_AXIS);
	else
	  c = n;
      }
    for (vsize i = dots.size (); i--;)
      {
	Grob *n = dots[i]->get_parent (Y_AXIS);
	n->relative_coordinate (c, X_AXIS);
      }
  }
  
  vector_sort (dots, position_less);
  for (vsize i = dots.size (); i--;)
    if (!dots[i]->is_live ())
      dots.erase (dots.begin () + i);

  Dot_configuration cfg;
  for (vsize i = 0;i < dots.size (); i++)
    {
      Dot_position dp;
      dp.dot_ = dots[i];

      Grob *note = dots[i]->get_parent (Y_AXIS);
      if (note)
	{
	  Grob *stem = unsmob_grob (note->get_object ("stem"));
	  if (stem)
	    dp.extremal_head_ = Stem::first_head (stem) == note;
	}

      int p = Staff_symbol_referencer::get_rounded_position (dp.dot_);

      /* icky, since this should go via a Staff_symbol_referencer
	 offset callback but adding a dot overwrites Y-offset. */
      p += (int) robust_scm2double (dp.dot_->get_property ("staff-position"), 0.0);
      dp.pos_ = p;

      if (dp.extremal_head_)
	dp.dir_ = to_dir (dp.dot_->get_property ("direction"));

      cfg.remove_collision (p);
      cfg[p] = dp;
      if (Staff_symbol_referencer::on_line (dp.dot_, p))
	cfg.remove_collision (p);
    }

  for (Dot_configuration::const_iterator i (cfg.begin ());
       i != cfg.end (); i++)
    {
      /*
	Junkme?
       */
      Staff_symbol_referencer::set_position (i->second.dot_, i->first);
    }
  return SCM_BOOL_T;
}

void
Dot_column::add_head (Grob *me, Grob *rh)
{
  Grob *d = unsmob_grob (rh->get_object ("dot"));
  if (d)
    {
      Side_position_interface::add_support (me, rh);

      Pointer_group_interface::add_grob (me, ly_symbol2scm ("dots"), d);
      d->set_property ("Y-offset", Grob::x_parent_positioning_proc);
      Axis_group_interface::add_element (me, d);
    }
}

ADD_INTERFACE (Dot_column,
	       
	       "Groups dot objects so they form a column, and position dots so they do not "
	       "clash with staff lines ",

	       /* properties */
	       "dots "
	       "positioning-done "
	       "direction "
	       );

