/*
  note-performer.cc -- implement Drum_note_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-context.hh"
#include "warn.hh"
#include "pitch.hh"
#include "music.hh"

class Drum_note_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Drum_note_performer);

protected:
  virtual bool try_music (Music *ev);
  void stop_translation_timestep ();
  void process_music ();

private:
  Link_array<Music> note_evs_;
  Link_array<Audio_note> notes_;
};

Drum_note_performer::Drum_note_performer ()
{
}

void
Drum_note_performer::process_music ()
{
  SCM tab = get_property ("drumPitchTable");

  while (note_evs_.size ())
    {
      Music *n = note_evs_.pop ();
      SCM sym = n->get_property ("drum-type");
      SCM defn = SCM_EOL;

      if (scm_is_symbol (sym)
	  && (scm_hash_table_p (tab) == SCM_BOOL_T))
	defn = scm_hashq_ref (tab, sym, SCM_EOL);

      if (Pitch *pit = unsmob_pitch (defn))
	{
	  Audio_note *p = new Audio_note (*pit, n->get_length (), 0);
	  Audio_element_info info (p, n);
	  announce_element (info);
	  notes_.push (p);
	}
    }

  note_evs_.clear ();
}

void
Drum_note_performer::stop_translation_timestep ()
{
  // why don't grace notes show up here?
  // --> grace notes effectively do not get delayed
  Moment now = now_mom ();
  for (int i = 0; i < notes_.size (); i++)
    play_element (notes_[i]);
  notes_.clear ();
  note_evs_.clear ();
}

bool
Drum_note_performer::try_music (Music *ev)
{
  if (ev->is_mus_type ("note-event"))
    {
      note_evs_.push (ev);
      return true;
    }
  else if (ev->is_mus_type ("busy-playing-event"))
    return note_evs_.size ();

  return false;
}

#include "translator.icc"

ADD_TRANSLATOR (Drum_note_performer,
		"Play drum notes.", "",
		"note-event busy-playing-event", "", "");
