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

Real
Rest_collision::force_shift_callback (Score_element const*them, Axis a)
{
  assert (a == Y_AXIS);

  Score_element * rc = unsmob_element (them->get_elt_pointer ("rest-collision"));

  if (rc)
    {
      /*
	Done: destruct pointers, so we do the shift only once.
      */
      SCM elts = rc->get_elt_pointer ("elements");
      rc->set_elt_pointer ("elements", SCM_EOL);

      do_shift (rc, elts);
    }
  
  return 0.0;
}

void
Rest_collision::add_column (Note_column *p)
{
  elt_l_->add_dependency (p);
  Pointer_group_interface gi (elt_l_);  
  gi.add_element (p);

  p->add_offset_callback (&Rest_collision::force_shift_callback, Y_AXIS);
  p->set_elt_pointer ("rest-collision", elt_l_->self_scm_);
}

/*
  these 3 have to go, because they're unnecessary complications.
 */
static Duration
to_duration (int type, int dots)
{
  Duration d;
  d.durlog_i_ = type;
  d.dots_i_ = dots;
  return d;
}

/*
  UGH

  elt_l_ should be "duration" independent
 */
static Moment
rhythmic_head2mom (Rhythmic_head* r)
{
  return to_duration (r->balltype_i (), r->dot_count ()).length_mom ();
}

/*
  ugh
 */
static Rhythmic_head*
col2rhythmic_head (Score_element* c)
{
  return dynamic_cast<Rhythmic_head*> (unsmob_element (c->get_elt_pointer ("rest")));
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
  Link_array<Note_column> rests;
  Link_array<Note_column> notes;

  for (SCM s = elts; gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element * e = unsmob_element (gh_car (s));
      if (e && unsmob_element (e->get_elt_pointer ("rest")))
	rests.push (dynamic_cast<Note_column*> (e));
      else
	notes.push (dynamic_cast<Note_column*> (e));
    }

  
  /* 
     handle rest-rest and rest-note collisions

     [todo]
     * decide not to print rest if too crowded?

     * ignore rests under beams.
   */

  // no rests to collide
  if (!rests.size())
    return SCM_UNDEFINED;

  // no partners to collide with
  if (rests.size() + notes.size () < 2)
    return SCM_UNDEFINED;

  // meisjes met meisjes
  if (!notes.size()) 
    {
      Moment m = rhythmic_head2mom (col2rhythmic_head (rests[0]));
      int i = 1;
      for (; i < rests.size (); i++)
	{
	  Moment me = rhythmic_head2mom (col2rhythmic_head (rests[i]));
	  if (me != m)
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
	    col2rhythmic_head (rests[i-1])
	      ->set_elt_property ("molecule-callback", SCM_BOOL_T);
	}
      else
	display_count = rests.size ();
      
      /*
	UGH.  Should get dims from table.  Should have minimum dist.
       */
      int dy = display_count > 2 ? 6 : 4;
      if (display_count > 1)
	{
	  rests[0]->translate_rests (dy);	
	  rests[1]->translate_rests (-dy);
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
      Note_column * rcol = rests[0];

      // try to be opposite of noteheads. 
      Direction dir = - notes[0]->dir();

      Interval restdim = rcol->rest_dim ();
      if (restdim.empty_b ())
	return SCM_UNDEFINED;
      
      // staff ref'd?
      Real staff_space = me->paper_l()->get_var ("interline");

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
	  notedim.unite (notes[i]->extent (Y_AXIS));
	}

      Interval inter (notedim);
      inter.intersect (restdim);

      Real dist =
	minimum_dist +  dir * (notedim[dir] - restdim[-dir]) >? 0;


      // FIXME
      //int stafflines = 5; // rcol->rests[0]->line_count;
      int stafflines = Staff_symbol_referencer_interface (me).line_count ();
      // hurg?
      stafflines = stafflines != 0 ? stafflines : 5;
      
      // move discretely by half spaces.
      int discrete_dist = int (ceil (dist / (0.5 *staff_space)));

      // move by whole spaces inside the staff.
      if (discrete_dist < stafflines+1)
	discrete_dist = int (ceil (discrete_dist / 2.0)* 2.0);
      
      rcol->translate_rests (dir * discrete_dist);
    }
  return SCM_UNDEFINED;
}

void
Rest_collision::set_interface ()
{
  elt_l_->set_extent_callback (0, X_AXIS);
  elt_l_->set_extent_callback (0, Y_AXIS);
  elt_l_->set_elt_pointer ("elements", SCM_EOL);
}

Rest_collision::Rest_collision (Score_element* c)
{
  elt_l_ = c;
}
