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

void
Rest_collision::add_column (Note_column *nc_l)
{
  add_dependency (nc_l);
  Group_interface gi (this);  
  if (nc_l->rest_b ())
    gi.name_ = "rests";
  else
    gi.name_ = "notes";
  
  gi.add_element (nc_l);
}

void
Rest_collision::do_pre_processing()
{
  Link_array<Note_column> rest_l_arr =
    Group_interface__extract_elements (this, (Note_column*) 0, "rests");
  Link_array<Note_column> ncol_l_arr =
    Group_interface__extract_elements (this, (Note_column*) 0, "notes");
				      
  
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
      /*
	UGH.  Should get dims from table.  Should have minimum dist.
       */
      int dy = rest_l_arr.size() > 2 ? 6 : 4;
	
      rest_l_arr[0]->translate_rests (rest_l_arr[0]->dir () *dy);	
      rest_l_arr.top()->translate_rests (rest_l_arr.top ()->dir ()* dy);
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
      int stafflines = 5; // rcol->rest_l_arr[0]->line_count;

      
      // move discretely by half spaces.
      int discrete_dist = int (ceil (dist / (0.5 *staff_space)));

      // move by whole spaces inside the staff.
      if (discrete_dist < stafflines+1)
	discrete_dist = int (ceil (discrete_dist / 2.0)* 2.0);
      
      rcol->translate_rests (dir * discrete_dist);
    }
}


Rest_collision::Rest_collision()
{
  set_elt_property ("rests", SCM_EOL);
  set_elt_property ("notes", SCM_EOL);
  set_elt_property ("transparent", SCM_BOOL_T);
  set_empty (X_AXIS);
  set_empty (Y_AXIS);
}

