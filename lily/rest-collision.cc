/*
  rest-collision.cc -- implement Rest_collision

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "rest-collision.hh"

#include <math.h>		// ceil.

#include "warn.hh"
#include "note-column.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "output-def.hh"
#include "rest.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "duration.hh"
#include "directional-element-interface.hh"

MAKE_SCHEME_CALLBACK (Rest_collision,force_shift_callback,2);
SCM
Rest_collision::force_shift_callback (SCM element_smob, SCM axis)
{
  Grob *them = unsmob_grob (element_smob);
  Axis a = (Axis) scm_to_int (axis);
  assert (a == Y_AXIS);

  if (Note_column::has_rests (them))
    {  
      Grob * rc = unsmob_grob (them->get_property ("rest-collision"));

      if (rc && !to_boolean (rc->get_property ("positioning-done")))
	{
	  rc->set_property ("positioning-done", SCM_BOOL_T);
	  do_shift (rc);
	}
    }  
  return scm_make_real (0.0);
}



void
Rest_collision::add_column (Grob*me,Grob *p)
{
  me->add_dependency (p);
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("elements"), p);

  /*
    only add callback for the rests, since we don't move anything
    else.

    (not?)
  */
  p->add_offset_callback (Rest_collision::force_shift_callback_proc, Y_AXIS);
  p->set_property ("rest-collision", me->self_scm ());
}



/*
  TODO: look at horizontal-shift to determine ordering between rests
  for more than two voices.
  
 */
SCM
Rest_collision::do_shift (Grob *me)
{
  SCM elts = me->get_property ("elements");

  Link_array<Grob> rests;
  Link_array<Grob> notes;

  for (SCM s = elts; scm_is_pair (s); s = scm_cdr (s))
    {
      Grob * e = unsmob_grob (scm_car (s));
      if (unsmob_grob (e->get_property ("rest")))
	{
	  /*
	    Ignore rests under beam.
	   */
	  Grob* st = unsmob_grob (e->get_property ("stem"));
	  if (st && unsmob_grob (st->get_property ("beam")))
	    continue;
	  
	  rests.push (e);
	}
      else
	notes.push (e);
    }

  
  /* 
     handle rest-rest and rest-note collisions

     [todo]
     * decide not to print rest if too crowded?
   */

  /*
    no partners to collide with
   */
  if (rests.size () + notes.size () < 2)
    return SCM_UNSPECIFIED;


  Real staff_space = Staff_symbol_referencer::staff_space (me);
  /*
    only rests
  */
  if (!notes.size ()) 
    {

      /*
	This is incomplete: in case of an uneven number of rests, the
	center one should be centered on the staff.
       */
      Drul_array< Link_array <Grob > > ordered_rests;
      for (int i = 0; i < rests.size (); i++)
	{
	  Grob * r = Note_column::get_rest (rests[i]);
	  
	  Direction d = get_grob_direction (r);
	  if (d)
	    {
	      ordered_rests[d].push (rests[i]);
	    }
	  else
	    rests[d]->warning (_("rest direction not set.  Cannot resolve collision."));
	}

      Direction d =  LEFT;
      do {
	ordered_rests[d].sort (Note_column::shift_compare);
      } while (flip (&d) != LEFT);
      
      do {
	if (ordered_rests[d].size () < 1)
	  {
	    if (ordered_rests[-d].size() > 1)
	      ordered_rests[-d][0]->warning (_ ("too many colliding rests"));
	  
	    return SCM_UNSPECIFIED;
	  }
      } while (flip (&d) != LEFT);

      Grob *common = common_refpoint_of_array (ordered_rests[DOWN], me, Y_AXIS);
      common =  common_refpoint_of_array (ordered_rests[UP], common, Y_AXIS);

      Real diff = 
	(ordered_rests[DOWN].top ()->extent (common, Y_AXIS)[UP]
	 - ordered_rests[UP].top ()->extent (common, Y_AXIS)[DOWN]) /staff_space;

      if (diff > 0)
	{
	  int amount_down = (int) ceil (diff / 2); 
	  diff -= amount_down;
	  Note_column::translate_rests (ordered_rests[DOWN].top (),
					-2 * amount_down);
	  if (diff > 0)
	    Note_column::translate_rests (ordered_rests[UP].top (),
					  2 * int (ceil (diff)));
	}

      do {
	for (int i = ordered_rests[d].size () -1; i-- > 0;)
	  {
	    Real last_y = ordered_rests[d][i+1]->extent (common, Y_AXIS)[d];
	    Real y = ordered_rests[d][i]->extent (common, Y_AXIS)[-d];

	    Real diff = d * ((last_y - y) /staff_space);
	    if (diff > 0)
	      Note_column::translate_rests (ordered_rests[d][i],d * (int) ceil (diff) * 2);
	  }
      } while (flip (&d) != LEFT);
    }
  else 
    {
      /*
	Rests and notes.
       */
      if (rests.size () > 1)
	{
	  warning (_ ("too many colliding rests"));
	}
      Grob * rcol = 0;
      Direction dir = CENTER;

      for (int i = rests.size (); !rcol && i--;)
	if (Note_column::dir (rests[i]))
	  {
	    dir = Note_column::dir (rests[i]);
	    rcol = rests[i];
	  }

      if (!rcol)
	return SCM_UNSPECIFIED;
      
      Grob *common = common_refpoint_of_array (notes, rcol, Y_AXIS);
      
      Interval restdim = rcol->extent (common, Y_AXIS);
      if (restdim.is_empty ())
	return SCM_UNSPECIFIED;
      
      Real staff_space = Staff_symbol_referencer::staff_space (rcol);
      Real minimum_dist = robust_scm2double (me->get_property ("minimum-distance"), 1.0) * staff_space;

      Interval notedim;
      for (int i = 0; i < notes.size (); i++) 
	{
	  notedim.unite (notes[i]->extent (common, Y_AXIS));
	}

      Real dist =
	minimum_dist +  dir * (notedim[dir] - restdim[-dir]) >? 0;

      int stafflines = Staff_symbol_referencer::line_count (me);
      if (!stafflines)
	{
	  programming_error ("No staff line count ? ");
	  stafflines =5;
	}
      
      // move discretely by half spaces.
      int discrete_dist = int (ceil (dist / (0.5 *staff_space)));

      // move by whole spaces inside the staff.
      if (discrete_dist < stafflines+1)
	discrete_dist = int (ceil (discrete_dist / 2.0)* 2.0);

      Note_column::translate_rests (rcol,dir * discrete_dist);
    }
  return SCM_UNSPECIFIED;
}


ADD_INTERFACE (Rest_collision,"rest-collision-interface",
	       "Move around ordinary rests (not multi-measure-rests) to avoid "
	       "conflicts.",
	       "minimum-distance positioning-done elements");

