/*
  rest-collision.cc -- implement Rest_collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <math.h>		// ceil.

#include "debug.hh"
#include "rest-collision.hh"
#include "note-column.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "paper-def.hh"
#include "rest.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "duration.hh"

MAKE_SCHEME_CALLBACK(Rest_collision,force_shift_callback,2);
SCM
Rest_collision::force_shift_callback (SCM element_smob, SCM axis)
{
  Score_element *them = unsmob_element (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  assert (a == Y_AXIS);

  Score_element * rc = unsmob_element (them->get_elt_property ("rest-collision"));

  if (rc)
    {
      /*
	Done: destruct pointers, so we do the shift only once.
      */
      SCM elts = rc->get_elt_property ("elements");
      rc->set_elt_property ("elements", SCM_EOL);

      do_shift (rc, elts);
    }
  
  return gh_double2scm (0.0);
}

void
Rest_collision::add_column (Score_element*me,Score_element *p)
{
  me->add_dependency (p);
  Pointer_group_interface::add_element (me, "elements", p);

  p->add_offset_callback (Rest_collision::force_shift_callback_proc, Y_AXIS);
  p->set_elt_property ("rest-collision", me->self_scm ());
}


/*
  Combination of dot-count and duration-log.
 */
static SCM
head_characteristic (Score_element * col)
{
  Score_element * s = unsmob_element (col->get_elt_property ("rest"));

  if (!s)
    return SCM_BOOL_F;
  else
    return gh_cons (s->get_elt_property ("duration-log"),
		    gh_int2scm (Rhythmic_head::dot_count (s)));
}

/*
  TODO: fixme, fucks up if called twice on the same set of rests.
 */
SCM
Rest_collision::do_shift (Score_element *me, SCM elts)
{
  /*
    ugh. -> score  elt type
   */
  Link_array<Score_element> rests;
  Link_array<Score_element> notes;

  for (SCM s = elts; gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * e = unsmob_element (gh_car (s));
      if (e && unsmob_element (e->get_elt_property ("rest")))
	rests.push (e);
      else
	notes.push (e);
    }

  
  /* 
     handle rest-rest and rest-note collisions

     [todo]
     * decide not to print rest if too crowded?

     * ignore rests under beams.
   */

  // no rests to collide
  if (!rests.size())
    return SCM_UNSPECIFIED;

  // no partners to collide with
  if (rests.size() + notes.size () < 2)
    return SCM_UNSPECIFIED;

  // meisjes met meisjes
  if (!notes.size()) 
    {
      SCM characteristic = head_characteristic  (rests[0]);
      int i = 1;
      for (; i < rests.size (); i++)
	{
	  if (!gh_equal_p (head_characteristic  (rests[i]), characteristic))
	    break;
	}

      /*
	If all durations are the same, we'll check if there are more
	rests than maximum-rest-count.
	Otherwise (different durations), we'll try to display them all
	(urg: all 3 of them, currently).
       */
      int display_count;
      SCM s = me->get_elt_property ("maximum-rest-count");
      if (i == rests.size ()
	  && gh_number_p (s) && gh_scm2int (s) < rests.size ())
	{
	  display_count = gh_scm2int (s);
	  for (; i > display_count; i--)
	    {
	      Score_element* r = unsmob_element (rests[i-1]->get_elt_property ("rest"));
	      if (r)
		r->suicide ();
	      rests[i-1]->suicide ();
	    }
	}
      else
	display_count = rests.size ();
      
      /*
	Ugh. Should have minimum dist.

	Ugh. What do we do if we have three different rests?
	
       */
      int dy = display_count > 2 ? 6 : 4; // FIXME Should get dims from table.
      if (display_count > 1)
	{
	  Direction d0 = Note_column::dir (rests[0]);
	  Direction d1 = Note_column::dir (rests[1]);	  

	  if (!d0 && !d1)
	    {
	      d0= UP;
	      d1 = DOWN;
	    }
	  else if (!d0)
	    d0 = - d1;
	  else if (!d1)
	    d1 = -d0;
		
	  Note_column::translate_rests (rests[0],d0 *dy);	
	  Note_column::translate_rests (rests[1], d1 *dy);
	}
    }
  // meisjes met jongetjes
  else 
    {
      if (rests.size () > 1)
	{
	  warning (_("too many colliding rests"));
	}
      if (notes.size () > 1)
	{
	  warning (_("too many notes for rest collision"));
	}
      Score_element * rcol = rests[0];

      // try to be opposite of noteheads. 
      Direction dir = - Note_column::dir (notes[0]);

      Interval restdim = Note_column::rest_dim (rcol);
      if (restdim.empty_b ())
	return SCM_UNSPECIFIED;
      
      // staff ref'd?
      Real staff_space = 1.0;

	/* FIXME
	  staff_space =  rcol->rests[0]->staff_space ();
	*/
      Real minimum_dist = gh_scm2double (me->get_elt_property ("minimum-distance")) * staff_space;
      
      /*
	assumption: ref points are the same. 
       */
      Interval notedim;
      for (int i = 0; i < notes.size(); i++) 
	{
	  notedim.unite (notes[i]->extent (notes[i],Y_AXIS)); // refp?
	}

      Interval inter (notedim);
      inter.intersect (restdim);

      Real dist =
	minimum_dist +  dir * (notedim[dir] - restdim[-dir]) >? 0;


      // FIXME
      //int stafflines = 5; // rcol->rests[0]->line_count;
      int stafflines = Staff_symbol_referencer::line_count (me);
      // hurg?
      stafflines = stafflines != 0 ? stafflines : 5;
      
      // move discretely by half spaces.
      int discrete_dist = int (ceil (dist / (0.5 *staff_space)));

      // move by whole spaces inside the staff.
      if (discrete_dist < stafflines+1)
	discrete_dist = int (ceil (discrete_dist / 2.0)* 2.0);
      
      Note_column::translate_rests (rcol,dir * discrete_dist);
    }
  return SCM_UNSPECIFIED;
}

void
Rest_collision::set_interface (Score_element*me)
{
  me->set_extent_callback (SCM_EOL, X_AXIS);
  me->set_extent_callback (SCM_EOL, Y_AXIS);
}

