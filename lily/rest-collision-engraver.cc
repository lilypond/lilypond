/*
  rest-collision-engraver.cc -- implement Rest_collision_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "warn.hh"
#include "engraver.hh"
#include "rest-collision.hh"
#include "note-column.hh"
#include "item.hh"

class Rest_collision_engraver : public Engraver
{
  Item *rest_collision_;
  vsize rest_count_;
  vector<Grob*> note_columns_;
protected:
  DECLARE_ACKNOWLEDGER (note_column);
  void process_acknowledged ();
  void stop_translation_timestep ();
public:
  TRANSLATOR_DECLARATIONS (Rest_collision_engraver);
};

Rest_collision_engraver::Rest_collision_engraver ()
{
  rest_collision_ = 0;
  rest_count_ = 0;
}

void
Rest_collision_engraver::process_acknowledged ()
{
  if (rest_collision_
      || note_columns_.empty ()
      || !rest_count_
      || (note_columns_.size () == rest_count_
	  && rest_count_ < 2))
    return;

  rest_collision_ = make_item ("RestCollision", SCM_EOL);

  for (vsize i = 0; i < note_columns_.size (); i++)
    Rest_collision::add_column (rest_collision_, note_columns_[i]);
}

void
Rest_collision_engraver::acknowledge_note_column (Grob_info i)
{
  note_columns_.push_back (i.grob ());
  if (Note_column::has_rests (i.grob ()))
    rest_count_++;
}

void
Rest_collision_engraver::stop_translation_timestep ()
{
  rest_collision_ = 0;
  note_columns_.clear ();
  rest_count_ = 0;
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Rest_collision_engraver, note_column);
ADD_TRANSLATOR (Rest_collision_engraver,
		/* doc */
		"Handle collisions of rests.",

		/* create */
		"RestCollision ",

		/* read */
		"",

		/* write */
		""
		);
