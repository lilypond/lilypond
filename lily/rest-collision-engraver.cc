/*
  rest-collision-engraver.cc -- implement Rest_collision_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include <list>

#include "duration.hh"
#include "engraver.hh"
#include "item.hh"
#include "moment.hh"
#include "note-column.hh"
#include "rest-collision.hh"
#include "stream-event.hh"
#include "warn.hh"

class Rest_collision_engraver : public Engraver
{
  Item *rest_collision_;
  vsize rest_count_;
  list<pair<Grob*, Moment> > note_columns_;
protected:
  DECLARE_ACKNOWLEDGER (note_column);
  void process_acknowledged ();
  void stop_translation_timestep ();
  void start_translation_timestep ();
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

  list<pair<Grob*, Moment> >::iterator i;
  for (i = note_columns_.begin (); i != note_columns_.end (); i++)
    Rest_collision::add_column (rest_collision_, i->first);
}

void
Rest_collision_engraver::acknowledge_note_column (Grob_info i)
{
  Moment end = now_mom ();
  if (Note_column::has_rests (i.grob ()))
    rest_count_++;
  else
    {
      // We only keep track of ending moments for columns with notes.
      // It is safe to add a column with notes to multiple RestCollisions, but
      // it might not be safe to add a column with rests to multiple RestCollisions.
      Grob *stem = Note_column::get_stem (i.grob ());
      Stream_event *ev = stem ? stem->event_cause () : 0;
      Duration *dur_ptr = ev ? unsmob_duration (ev->get_property ("duration")) : 0;
      if (dur_ptr)
	{
	  if (end.grace_part_)
	    end.grace_part_ += dur_ptr->get_length ();
	  else
	    end.main_part_ += dur_ptr->get_length ();
	}
    }
  note_columns_.push_back (pair<Grob*, Moment> (i.grob (), end));
}

void
Rest_collision_engraver::stop_translation_timestep ()
{
  rest_collision_ = 0;
  rest_count_ = 0;
}

void
Rest_collision_engraver::start_translation_timestep ()
{
  list<pair<Grob*, Moment> >::iterator i = note_columns_.begin ();
  while (i != note_columns_.end ())
    {
      if (i->second <= now_mom ())
	i = note_columns_.erase (i);
      else
	i++;
    }
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
