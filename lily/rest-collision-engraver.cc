/*
  rest-collision-reg.cc -- implement Rest_collision_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "rest-collision.hh"
#include "engraver.hh"
#include "collision.hh"
#include "note-column.hh"

class Rest_collision_engraver : public Engraver
{
  Item* rest_collision_p_;

  Link_array<Note_column> note_column_l_arr_;
protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_pre_move_processing();
public:
  VIRTUAL_COPY_CONS(Translator);
  Rest_collision_engraver();
  
};

ADD_THIS_TRANSLATOR(Rest_collision_engraver);

Rest_collision_engraver::Rest_collision_engraver()
{
  rest_collision_p_ =0;
}

void
Rest_collision_engraver::process_acknowledged ()
{
  if (rest_collision_p_ || note_column_l_arr_.size () < 2)
    return;

  rest_collision_p_ = new Item (get_property ("basicRestCollisionProperties"));
  Rest_collision::set_interface (rest_collision_p_);
  announce_element (Score_element_info (rest_collision_p_, 0));
  for (int i=0; i< note_column_l_arr_.size (); i++)
    Rest_collision::add_column ( rest_collision_p_,note_column_l_arr_[i]);
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
