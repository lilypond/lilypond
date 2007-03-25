/*
  dot-column.cc -- implement Dot_column

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "dot-column.hh"

#include <cstdio>
#include <cmath>
#include <map>
#include <set>

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
#include "note-head.hh"
#include "skyline.hh"
#include "dot-formatting-problem.hh"

MAKE_SCHEME_CALLBACK (Dot_column, calc_positioning_done, 1);
SCM
Dot_column::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);  

  me->set_property ("positioning-done", SCM_BOOL_T);

  vector<Grob*> dots
    = extract_grob_array (me, "dots");

  vector<Grob*> main_heads;

  Grob *commonx = me;
  { /*
      Trigger note collision resolution first, since that may kill off
      dots when merging.
    */
    for (vsize i = 0; i < dots.size (); i++)
      {
	Grob *n = dots[i]->get_parent (Y_AXIS);
	commonx = n->common_refpoint (commonx, X_AXIS);

	if (Grob *stem = unsmob_grob (n->get_object("stem")))
	  {
	    commonx = stem->common_refpoint (commonx, X_AXIS);

	    if (Stem::first_head (stem) == n)
	      main_heads.push_back (n);
	  }
      }
  }

  vector<Box> boxes;
  set<Grob*> stems;

  extract_grob_set(me, "side-support-elements", support);

  Interval base_x;
  for (vsize i = 0; i < main_heads.size (); i++)
    base_x.unite (main_heads[i]->extent (commonx, X_AXIS));
 
  for (vsize i = 0; i < support.size (); i++)
    {
      if (Note_head::has_interface (support[i]))
	{
	  Interval y(-1, 1);
	  y += Staff_symbol_referencer::get_position (support[i]);
	  
	  Box b (support[i]->extent (commonx, X_AXIS), y);
	  boxes.push_back (b);

	  if (Grob *s = unsmob_grob (support[i]->get_object ("stem")))
	    stems.insert (s);
	}
      else
	programming_error ("unknown grob in dot col support");
    }


  for (set<Grob*>::const_iterator i(stems.begin());
       i != stems.end (); i++)
    {
      Grob *stem = (*i);
      Stencil flag = Stem::flag (stem);
      if (!flag.is_empty ())
	{
	  Interval y = flag.extent (Y_AXIS)
	    * (2 / Staff_symbol_referencer::staff_space (stem))
	    + Stem::stem_end_position (stem);
		  
	  Interval x = stem->relative_coordinate (commonx, X_AXIS)
	    + flag.extent (X_AXIS);

	  boxes.push_back (Box (x,y));
	}
    }
	      
  vector_sort (dots, position_less);
  for (vsize i = dots.size (); i--;)
    if (!dots[i]->is_live ())
      dots.erase (dots.begin () + i);

  Dot_formatting_problem problem (boxes, base_x);

  Dot_configuration cfg (problem);
  for (vsize i = 0; i < dots.size (); i++)
    {
      Dot_position dp;
      dp.dot_ = dots[i];

      Grob *note = dots[i]->get_parent (Y_AXIS);
      if (note)
	{
	  Grob *stem = unsmob_grob (note->get_object ("stem"));
	  if (stem)
	    {
	      dp.extremal_head_ = Stem::first_head (stem) == note;


	    }
	  
	  dp.x_extent_ = note->extent (commonx, X_AXIS);
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

  problem.register_configuration (cfg);

  for (Dot_configuration::const_iterator i (cfg.begin ());
       i != cfg.end (); i++)
    {
      /*
	Junkme?
       */
      Staff_symbol_referencer::set_position (i->second.dot_, i->first);
    }

  
  me->translate_axis (cfg.x_offset () - me->relative_coordinate (commonx, X_AXIS),
		      X_AXIS);
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
      d->set_property ("X-offset", Grob::x_parent_positioning_proc);
      Axis_group_interface::add_element (me, d);
    }
}

ADD_INTERFACE (Dot_column,
	       
	       "Groups dot objects so they form a column, and position dots so they do not "
	       "clash with staff lines. ",

	       /* properties */
	       "dots "
	       "positioning-done "
	       "direction "
	       );

