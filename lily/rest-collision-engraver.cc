/*
  rest-collision-reg.cc -- implement Rest_collision_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "warn.hh"
#include "rest-collision.hh"
#include "engraver.hh"
#include "note-collision.hh"
#include "note-column.hh"

class Rest_collision_engraver : public Engraver
{
  Item* rest_collision_;

  Link_array<Grob> note_columns_;
protected:
  virtual void acknowledge_grob (Grob_info);
  virtual void process_acknowledged_grobs ();
  virtual void stop_translation_timestep ();
public:
  TRANSLATOR_DECLARATIONS(Rest_collision_engraver);  
};



Rest_collision_engraver::Rest_collision_engraver ()
{
  rest_collision_ =0;
}

void
Rest_collision_engraver::process_acknowledged_grobs ()
{
  if (rest_collision_ || note_columns_.size () < 2)
    return;

  rest_collision_ = new Item (get_property ("RestCollision"));

  announce_grob(rest_collision_, SCM_EOL);
  for (int i=0; i< note_columns_.size (); i++)
    Rest_collision::add_column (rest_collision_,note_columns_[i]);
}

void
Rest_collision_engraver::acknowledge_grob (Grob_info i)
{
  if (Note_column::has_interface (i.grob_))
    note_columns_.push (i.grob_);
}

void
Rest_collision_engraver::stop_translation_timestep ()
{
  if (rest_collision_) 
    {
      typeset_grob (rest_collision_);
      rest_collision_ = 0;
    }
  note_columns_.clear ();
}
ENTER_DESCRIPTION(Rest_collision_engraver,
/* descr */       "Handles collisions of rests.",
/* creats*/       "RestCollision",
/* acks  */       "note-column-interface",
/* reads */       "",
/* write */       "");
