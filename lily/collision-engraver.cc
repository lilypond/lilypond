/*
  collision-reg.cc -- implement Collision_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "note-column.hh"
#include "collision.hh"

#include "engraver.hh"
#include "axis-group-interface.hh"

/*
  collect Note_column, and as soon as there are 2 or more, put them in
  a collision object.  */
class Collision_engraver : public Engraver {
  Item * col_p_;
  Link_array<Grob> note_column_l_arr_;

protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void create_grobs ();
  virtual void stop_translation_timestep ();
public:
  TRANSLATOR_DECLARATIONS(Collision_engraver);
};


void
Collision_engraver::create_grobs ()
{
  if (col_p_ || note_column_l_arr_.size () < 2)
    return ;
  if (!col_p_) 
    {
      col_p_ = new Item (get_property ("NoteCollision"));
      Axis_group_interface::set_interface (col_p_);
      Axis_group_interface::set_axes (col_p_, X_AXIS, Y_AXIS);

      announce_grob (col_p_,0);
    }
  
  for (int i=0; i< note_column_l_arr_.size (); i++)
    Collision::add_column (col_p_,note_column_l_arr_[i]);
}

void
Collision_engraver::acknowledge_grob (Grob_info i)
{
  if (Note_column::has_interface (i.grob_l_))
    {
      /*should check Y axis? */
      if (Note_column::rest_b (i.grob_l_) || i.grob_l_->parent_l (X_AXIS))
	return ;

      note_column_l_arr_.push (i.grob_l_);
    }
}

void
Collision_engraver::stop_translation_timestep ()
{
  if (col_p_) 
    {
      typeset_grob (col_p_);
      col_p_ =0;
    }
  note_column_l_arr_.clear ();
}

Collision_engraver::Collision_engraver ()
{
  col_p_ =0;
}




ENTER_DESCRIPTION(Collision_engraver,
/* descr */       "",
/* creats*/       "NoteCollision",
/* acks  */       "note-column-interface",
/* reads */       "",
/* write */       "");
