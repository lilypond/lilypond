/*
  rest-collision.cc -- implement Rest_collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "rest-collision.hh"
#include "note-column.hh"
#include "stem.hh"
#include "note-head.hh"
#include "collision.hh"
#include "paper-def.hh"




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
Rest_collision::do_post_processing()
{
  /*
    [TODO]
    handle rest under beam (do_post: beams are calculated now)
    what about combination of collisions and rest under beam
   */
}

void
Rest_collision::do_pre_processing()
{
  /* 
     handle rest-rest and rest-note collisions

     [todo]
     decide not to print rest if too crowded?
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
      int dy = rest_l_arr_.size() > 2 ? 6 : 4;
	
      rest_l_arr_[0]->translate_rests (rest_l_arr_[0]->dir_ *dy);	
      // top is last element...
      rest_l_arr_.top()->translate_rests (rest_l_arr_.top ()->dir_* dy);	
    }
  // meisjes met jongetjes
  else 
    {
      // int dir_i = - ncol_l_arr_[0]->dir_;
      int dir_i = rest_l_arr_[0]->dir_;
	
      // minimum move
      int minpos = 4;
	
      // quart rest height
      // UGH Should get dims from table!
      int size_i = 6;
	
      Real internote_f = paper ()->internote_f ();
      int sep_i = 3 + size_i / 2;
      for (int i = 0; i < ncol_l_arr_.size(); i++) 
	{
	  // how to know whether to sort?
	  ncol_l_arr_[i]->sort();
	  for (int j = 0; j < ncol_l_arr_[i]->head_l_arr_.size(); j++)
	    {
	      int stem = (int)((ncol_l_arr_[i]->stem_l_->extent
			       (Y_AXIS)[dir_i]) / internote_f);
	      minpos = minpos >? (dir_i * stem + sep_i);
	    }
	}
      rest_l_arr_[0]->translate_rests (dir_i * minpos);	
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
Rest_collision::do_substitute_dependency (Score_element*o,Score_element*n)
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
  transparent_b_ = true;
  set_empty (true);
}
