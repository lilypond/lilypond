/*
  rest-collision-reg.cc -- implement Rest_collision_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "rest-collision.hh"
#include "rest-collision-engraver.hh"
#include "collision.hh"
#include "note-column.hh"



ADD_THIS_TRANSLATOR(Rest_collision_engraver);

Rest_collision_engraver::Rest_collision_engraver()
{
  rest_collision_p_ =0;
}

void
Rest_collision_engraver::process_acknowledged ()
{
  if (rest_collision_p_ ||   note_column_l_arr_.size () < 2)
    return;

  rest_collision_p_ = new Rest_collision;
  announce_element (Score_element_info (rest_collision_p_, 0));
  for (int i=0; i< note_column_l_arr_.size (); i++)
    rest_collision_p_->add_column (note_column_l_arr_[i]);
}

void
Rest_collision_engraver::acknowledge_element (Score_element_info i)
{
  if (dynamic_cast<Note_column *> (i.elem_l_))
    note_column_l_arr_.push (dynamic_cast<Note_column *> (i.elem_l_));
}

void
Rest_collision_engraver::do_pre_move_processing()
{
  if (rest_collision_p_) 
    {
      typeset_element (rest_collision_p_);
      rest_collision_p_ = 0;
    }
  note_column_l_arr_.clear ();
}
