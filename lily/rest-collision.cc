/*
  rest-collision.cc -- implement Rest_collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>		// ceil.

#include "beam.hh"
#include "debug.hh"
#include "rest-collision.hh"
#include "note-column.hh"
#include "stem.hh"
#include "note-head.hh"
#include "collision.hh"
#include "paper-def.hh"
#include "rest.hh"
#include "group-interface.hh"
#include "staff-symbol-referencer.hh"
#include "duration.hh"

void
Rest_collision::add_column (Note_column *nc_l)
{
  add_dependency (nc_l);
  Pointer_group_interface gi (this);  
  if (nc_l->rest_b ())
    gi.name_ = "rests";
  else
    gi.name_ = "notes";
  
  gi.add_element (nc_l);
}

static Duration
to_duration (int type, int dots)
{
  Duration d;
  d.durlog_i_ = type;
  d.dots_i_ = dots;
  return d;
}

static Moment
rhythmic_head2mom (Rhythmic_head* r)
{
  return to_duration (r->balltype_i (), r->dot_count ()).length_mom ();
}

/*
  ugh
 */
static Rhythmic_head*
col2rhythmic_head (Note_column* c)
{
  SCM s = c->get_elt_pointer ("rests");
  assert (gh_pair_p (s));
  Score_element* e = unsmob_element (gh_car (s));
  return dynamic_cast<Rhythmic_head*> (e);
}

void
Rest_collision::before_line_breaking ()
{
  Link_array<Note_column> rest_l_arr =
    Pointer_group_interface__extract_elements (this, (Note_column*) 0, "rests");
  Link_array<Note_column> ncol_l_arr =
    Pointer_group_interface__extract_elements (this, (Note_column*) 0, "notes");
				      
  
  /* 
     handle rest-rest and rest-note collisions

     [todo]
     * decide not to print rest if too crowded?

     * ignore rests under beams.
   */

  // no rests to collide
  if (!rest_l_arr.size())
    return;

  // no partners to collide with
  if (rest_l_arr.size() + ncol_l_arr.size () < 2)
    return;

  // meisjes met meisjes
  if (!ncol_l_arr.size()) 
    {
      Moment m = rhythmic_head2mom (col2rhythmic_head (rest_l_arr[0]));
      int i = 1;
      for (; i < rest_l_arr.size (); i++)
	{
	  Moment me = rhythmic_head2mom (col2rhythmic_head (rest_l_arr[i]));
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
      SCM s = get_elt_property ("maximum-rest-count");
      if (i == rest_l_arr.size ()
	  && gh_number_p (s) && gh_scm2int (s) < rest_l_arr.size ())
	{
	  display_count = gh_scm2int (s);
	  for (; i > display_count; i--)
	    col2rhythmic_head (rest_l_arr[i-1])
	      ->set_elt_property ("molecule-callback", SCM_BOOL_T);
	}
      else
	display_count = rest_l_arr.size ();
      
      /*
	UGH.  Should get dims from table.  Should have minimum dist.
       */
      int dy = display_count > 2 ? 6 : 4;
      if (display_count > 1)
	{
	  rest_l_arr[0]->translate_rests (dy);	
	  rest_l_arr[1]->translate_rests (-dy);
	}
    }
  // meisjes met jongetjes
  else 
    {
      if (rest_l_arr.size () > 1)
	{
	  warning (_("too many colliding rests"));
	}
      if (ncol_l_arr.size () > 1)
	{
	  warning (_("too many notes for rest collision"));
	}
      Note_column * rcol = rest_l_arr[0];

      // try to be opposite of noteheads. 
      Direction dir = - ncol_l_arr[0]->dir();

      Interval restdim = rcol->rest_dim ();
      if (restdim.empty_b ())
	return;
      
      // staff ref'd?
      Real staff_space = paper_l()->get_var ("interline");

	/* FIXME
	  staff_space =  rcol->rest_l_arr[0]->staff_space ();
	*/
      Real half_staff_space_f = staff_space/2;
      Real minimum_dist = paper_l ()->get_var ("restcollision_minimum_dist")
	* half_staff_space_f;
      
      /*
	assumption: ref points are the same. 
       */
      Interval notedim;
      for (int i = 0; i < ncol_l_arr.size(); i++) 
	{
	  notedim.unite (ncol_l_arr[i]->extent (Y_AXIS));
	}

      Interval inter (notedim);
      inter.intersect (restdim);

      Real dist =
	minimum_dist +  dir * (notedim[dir] - restdim[-dir]) >? 0;


      // FIXME
      //int stafflines = 5; // rcol->rest_l_arr[0]->line_count;
      int stafflines = Staff_symbol_referencer_interface (this).line_count ();
      // hurg?
      stafflines = stafflines != 0 ? stafflines : 5;
      
      // move discretely by half spaces.
      int discrete_dist = int (ceil (dist / (0.5 *staff_space)));

      // move by whole spaces inside the staff.
      if (discrete_dist < stafflines+1)
	discrete_dist = int (ceil (discrete_dist / 2.0)* 2.0);
      
      rcol->translate_rests (dir * discrete_dist);
    }
}


Rest_collision::Rest_collision(SCM s)
  : Item (s)
{
  set_elt_pointer ("rests", SCM_EOL);
  set_elt_pointer ("notes", SCM_EOL);
  set_extent_callback (0, X_AXIS);
  set_extent_callback (0, Y_AXIS);
}

