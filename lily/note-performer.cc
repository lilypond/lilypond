/*
  note-performer.cc -- implement Note_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-context.hh"
#include "warn.hh"
#include "music.hh"

/**
   Convert evs to audio notes.
*/
class Note_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Note_performer);

protected:
  virtual bool try_music (Music *ev);

  void stop_translation_timestep ();
  void process_music ();

private:
  Link_array__Music_ note_evs_;
  Link_array__Audio_note_ notes_;
};

void
Note_performer::process_music ()
{
  if (note_evs_.size ())
    {
      int transposing = 0;

      SCM prop = get_property ("instrumentTransposition");
      if (unsmob_pitch (prop))
	transposing = unsmob_pitch (prop)->semitone_pitch ();

      while (note_evs_.size ())
	{
	  Music *n = note_evs_.back ();
	  note_evs_.pop_back ();
	  SCM pit = n->get_property ("pitch");

	  if (Pitch *pitp = unsmob_pitch (pit))
	    {
	      Audio_note *p = new Audio_note (*pitp, n->get_length (), - transposing);
	      Audio_element_info info (p, n);
	      announce_element (info);
	      notes_.push_back (p);
	    }
	}
      note_evs_.clear ();
    }
}

void
Note_performer::stop_translation_timestep ()
{
  // why don't grace notes show up here?
  // --> grace notes effectively do not get delayed
  Moment now = now_mom ();
  for (vsize i = 0; i < notes_.size (); i++)
    play_element (notes_[i]);
  notes_.clear ();
  note_evs_.clear ();
}

bool
Note_performer::try_music (Music *ev)
{
  if (ev->is_mus_type ("note-event"))
    {
      note_evs_.push_back (ev);
      return true;
    }
  else if (ev->is_mus_type ("busy-playing-event"))
    return note_evs_.size ();

  return false;
}

#include "translator.icc"

ADD_TRANSLATOR (Note_performer, "", "",
		"note-event "
		"busy-playing-event",
		"", "");

Note_performer::Note_performer ()
{
}
