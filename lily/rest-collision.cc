/*
  rest-collision.cc -- implement Rest_collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "rest-collision.hh"
#include "note-column.hh"
#include "stem.hh"
#include "note-head.hh"
#include "collision.hh"
#include "paper-def.hh"


IMPLEMENT_IS_TYPE_B1(Rest_collision,Item);

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
    handle rest under beam (do_post: beams are calculated now)

    [todo]
    i-d like to have access to the beam itself, 
    iso only the (half-initialised?) stem

    what about combination of collisions and rest under beam
   */

  // no rests to collide
  if (!rest_l_arr_.size())
  	return;
  // can this happen?
  Stem* stem_l = rest_l_arr_[0]->stem_l_;
  if (!stem_l)
    return;
  // no beam
  if (!(stem_l->beams_left_i_ || stem_l->beams_right_i_))
    return;

  int dir_i = rest_l_arr_[0]->dir_;
  int midpos = 4;
  // ugh
  int stem_length_i = 7 - 2;
  // ugh, Stem::stem_start vs Stem::stem_end
  int pos = (int)(stem_l->stem_end_f() - midpos) - dir_i * stem_length_i;
  /*
    nogo: stem_start not set for rests?
  int pos = (stem_l->stem_begin_f() - midpos) + dir_i * 2;

  WHY IS THIS STILL HERE? --hwn
  */
  rest_l_arr_[0]->translate_rests (pos);	
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
      // hope it's 4: if it works->doco
      int midpos = 0;
	
      // minimum move
      int minpos = 4;
	
      // quart rest height
      // UGH Should get dims from table!
      int size_i = 6;
	
      int sep_i = 3 + size_i / 2;
      for (int i = 0; i < ncol_l_arr_.size(); i++) 
	{
	  // how to know whether to sort?
	  ncol_l_arr_[i]->sort();
	  for (int j = 0; j < ncol_l_arr_[i]->head_l_arr_.size(); j++)
	    minpos = minpos >? dir_i * 
	      (ncol_l_arr_[i]->head_l_arr_[j]->position_i_ -midpos) + sep_i;
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
  Item*o_l = o->access_Item ();
  

  if (o_l&&o_l->is_type_b (Note_column::static_name ()))
    {
      Note_column *n_l = n?(Note_column*)n->access_Item ():0;
      rest_l_arr_.substitute ((Note_column*)o_l, n_l);
      ncol_l_arr_.substitute ((Note_column*)o_l, n_l);
    }
}

Rest_collision::Rest_collision()
{
  transparent_b_ = true;
  set_empty (true);
}
