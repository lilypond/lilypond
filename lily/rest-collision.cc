/*
  rest-collision.cc -- implement Rest_collision

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "rest-collision.hh"
#include "rest-column.hh"
#include "note-column.hh"
#include "stem.hh"
#include "note-head.hh"
#include "collision.hh"
#include "paper-def.hh"

IMPLEMENT_STATIC_NAME(Rest_collision);
IMPLEMENT_IS_TYPE_B1(Rest_collision,Item);

void
Rest_collision::add(Note_column *nc_l)
{
    add_dependency(nc_l);
    ncol_l_arr_.push(nc_l);
}

void
Rest_collision::add(Rest_column *rc_l)
{
    add_dependency(rc_l);
    rest_l_arr_.push(rc_l);
}

void
Rest_collision::add(Collision * c_l)
{
    add_dependency(c_l);
    for (int i=0; i < c_l->clash_l_arr_.size(); i ++)
	ncol_l_arr_.push(c_l->clash_l_arr_[i]);
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

    Real inter_f = paper()->internote_f();
    int dir_i = rest_l_arr_[0]->dir_i_;
    int midpos = 4;
#if 1
    // ugh
    int stem_length_i = 7 - 2;
    // ugh, Stem::stem_start vs Stem::stem_end
    int pos = (stem_l->stem_end_f() - midpos) - dir_i * stem_length_i;
#else // nogo: stem_start not set for rests?
    int pos = (stem_l->stem_start_f() - midpos) + dir_i * 2;
#endif
    Real dy = pos * inter_f;
    rest_l_arr_[0]->translate_y(dy);	
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
    if (rest_l_arr_.size() + ncol_l_arr_.size() < 2 )
	return;

    Real inter_f = paper()->internote_f();

    // meisjes met meisjes
    if (!ncol_l_arr_.size()) {
	Real dy = rest_l_arr_.size() > 2 ? 6 * inter_f : 4 * inter_f;
	rest_l_arr_[0]->translate_y(dy);	
	// top is last element...
	rest_l_arr_.top()->translate_y(-dy);	
    }
    // meisjes met jongetjes
    else {
#if 0 // breendet: rests go always under
	// geen gemug, trug op je rug
	int dir_i = -1;
	rest_l_arr_[0]->translate_y(dir_i * 3 * inter_f);	
#else
	// int dir_i = - ncol_l_arr_[0]->dir_i_;
	int dir_i = rest_l_arr_[0]->dir_i_;
	// hope it's 4: if it works->doco
	int midpos = 4;
	// minimum move
	int minpos = 4;
	// quart rest height
	int size_i = 6;
	int sep_i = 3 + size_i / 2;
	for (int i = 0; i < ncol_l_arr_.size(); i++) {
	    // how to know whether to sort?
	    ncol_l_arr_[i]->sort();
	    for ( int j = 0; j < ncol_l_arr_[i]->head_l_arr_.size(); j++ )
		minpos = minpos >? dir_i * ( ncol_l_arr_[i]->head_l_arr_[j]->position_i_ - midpos ) + sep_i;
	}
	Real dy = dir_i * minpos * inter_f;
	rest_l_arr_[0]->translate_y(dy);	
#endif
    }
}

void
Rest_collision::do_print() const
{
#ifndef NPRINT
    mtor << "rests: " << rest_l_arr_.size() << ", ";
    mtor << "cols: " << ncol_l_arr_.size();
#endif
}

void
Rest_collision::do_substitute_dependency(Score_elem*o,Score_elem*n)
{
    Item*o_l = o->item();
    Item*n_l = n?n->item():0;
    
    rest_l_arr_.substitute((Rest_column*)o_l,(Rest_column*)n_l);
    ncol_l_arr_.substitute((Note_column*)o_l,(Note_column*)n_l);
}

Rest_collision::Rest_collision()
{
    transparent_b_ = true;
    empty_b_ = true;
}
