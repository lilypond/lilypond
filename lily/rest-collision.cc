/*
  rest-collision.cc -- implement Rest_collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "beam.hh"
#include "debug.hh"
#include "rest-collision.hh"
#include "note-column.hh"
#include "stem.hh"
#include "note-head.hh"
#include "collision.hh"
#include "paper-def.hh"
#include "rest.hh"

void
Rest_collision::add_column (Note_column *nc_l)
{
  add_dependency (nc_l);
  if (nc_l->rest_b ())
    rest_l_arr_.push (nc_l);
  else
    ncol_l_arr_.push (nc_l);
}

void
Rest_collision::do_pre_processing()
{
  /* 
     handle rest-rest and rest-note collisions

     [todo]
     * decide not to print rest if too crowded?

     * ignore rests under beams.
   */

  // no rests to collide
  if (!rest_l_arr_.size())
    return;

  // no partners to collide with
  if (rest_l_arr_.size() + ncol_l_arr_.size () < 2)
    return;

  // meisjes met meisjes
  if (!ncol_l_arr_.size()) 
    {
      /*
	UGH.  Should get dims from table.  Should have minimum dist.
       */
      int dy = rest_l_arr_.size() > 2 ? 6 : 4;
	
      rest_l_arr_[0]->translate_rests (rest_l_arr_[0]->dir () *dy);	
      rest_l_arr_.top()->translate_rests (rest_l_arr_.top ()->dir ()* dy);
    }
  // meisjes met jongetjes
  else 
    {
      if (rest_l_arr_.size () > 1)
	{
	  warning (_("Too many colliding rests."));
	}
      if (ncol_l_arr_.size () > 1)
	{
	  warning (_("Too many notes for rest collision."));
	}
      Note_column * rcol = rest_l_arr_[0];

      // try to be opposite of noteheads. 
      Direction dir = - ncol_l_arr_[0]->dir();

      Interval restdim;
      for (int i=0; i < rcol->rest_l_arr_.size(); i++)
	restdim.unite (rcol->rest_l_arr_[i]->extent (Y_AXIS));

      if (restdim.empty_b ())
	return;
      
      // staff ref'd?
      Real staff_space = rcol->rest_l_arr_[0]->staff_line_leading_f ();      
      Real internote_f = staff_space/2;
      Real minimum_dist = paper_l ()->get_var ("restcollision_minimum_dist")
	* internote_f;
      
      /*
	assumption: ref points are the same. 
       */
      Interval notedim;
      for (int i = 0; i < ncol_l_arr_.size(); i++) 
	{
	  notedim.unite (ncol_l_arr_[i]->extent (Y_AXIS));
	}

      Interval inter (notedim);
      inter.intersect (restdim);

      Real dist =
	minimum_dist +  dir * (notedim[dir] - restdim[-dir]) >? 0;


      int stafflines = rcol->rest_l_arr_[0]->lines_i ();

      
      // move discretely by half spaces.
      int discrete_dist = int (ceil (dist / (0.5 *staff_space)));

      // move by whole spaces inside the staff.
      if (discrete_dist < stafflines+1)
	discrete_dist = int (ceil (discrete_dist / 2.0)* 2.0);
      
      rcol->translate_rests (dir * discrete_dist);
    }
}

void
Rest_collision::do_print() const
{
#ifndef NPRINT
  DOUT << "rests: " << rest_l_arr_.size() << ", ";
  DOUT << "cols: " << ncol_l_arr_.size();
#endif
}

void
Rest_collision::do_substitute_element_pointer (Score_element*o,Score_element*n)
{
  if (Note_column *onl = dynamic_cast<Note_column *> (o))
    {
      Note_column *n_l = n?dynamic_cast<Note_column *> (n):0;
      rest_l_arr_.substitute (onl, n_l);
      ncol_l_arr_.substitute (onl, n_l);
    }
}

Rest_collision::Rest_collision()
{
  set_elt_property (transparent_scm_sym, SCM_BOOL_T);
  set_empty (true, X_AXIS, Y_AXIS);
}
