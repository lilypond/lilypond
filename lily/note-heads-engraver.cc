/*
  note-heads-engraver.cc -- part of GNU LilyPond

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <ctype.h>

#include "rhythmic-head.hh"
#include "output-def.hh"
#include "event.hh"
#include "dots.hh"
#include "dot-column.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"
#include "engraver.hh"
#include "warn.hh"

class Note_heads_engraver : public Engraver
{
  Link_array<Item> notes_;
  Link_array<Item> dots_;
  Link_array<Music> note_reqs_;

public:
  TRANSLATOR_DECLARATIONS (Note_heads_engraver);

protected:
  virtual bool try_music (Music *req) ;
  virtual void process_music ();

  virtual void stop_translation_timestep ();
};

Note_heads_engraver::Note_heads_engraver ()
{
}

bool
Note_heads_engraver::try_music (Music *m) 
{
  if (m->is_mus_type ("note-event"))
    {
      note_reqs_.push (m);
      return true;
    }
  else if (m->is_mus_type ("busy-playing-event"))
    return note_reqs_.size ();
  else if (m->is_mus_type ("start-playing-event"))
    return note_reqs_.size ();
  
  return false;
}


void
Note_heads_engraver::process_music ()
{
  for (int i=0; i < note_reqs_.size (); i++)
    {

      Music * req = note_reqs_[i];
      Item *note = make_item ("NoteHead", req->self_scm ());
      
      Duration dur = *unsmob_duration (req->get_property ("duration"));

      note->set_property ("duration-log", scm_int2num (dur.duration_log ()));
      if (dur.dot_count ())
	{
	  Item * d = make_item ("Dots", note->self_scm ());
	  Rhythmic_head::set_dots (note, d);
	  
	  if (dur.dot_count ()
	      != robust_scm2int (d->get_property ("dot-count"), 0))
	    d->set_property ("dot-count", scm_int2num (dur.dot_count ()));

	  d->set_parent (note, Y_AXIS);
	  
	  dots_.push (d);
	}

      Pitch *pit =unsmob_pitch (req->get_property ("pitch"));

      int pos = pit ? pit->steps () : 0;
      SCM c0 = get_property ("middleCPosition");
      if (ly_c_number_p (c0))
	pos += scm_to_int (c0);

      note->set_property ("staff-position",   scm_int2num (pos));
      notes_.push (note);
    }
}

void
Note_heads_engraver::stop_translation_timestep ()
{

  notes_.clear ();
  dots_.clear ();
  note_reqs_.clear ();
}



ENTER_DESCRIPTION (Note_heads_engraver,
/* descr */       "Generate noteheads.",
/* creats*/       "NoteHead Dots",
/* accepts */     "note-event busy-playing-event",
/* acks  */      "",
/* reads */       "middleCPosition",
/* write */       "");
