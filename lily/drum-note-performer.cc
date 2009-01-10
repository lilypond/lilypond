/*
  note-performer.cc -- implement Drum_note_performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "performer.hh"
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-context.hh"
#include "stream-event.hh"
#include "translator.icc"
#include "warn.hh"

class Drum_note_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Drum_note_performer);

protected:
  void stop_translation_timestep ();
  void process_music ();
  DECLARE_TRANSLATOR_LISTENER (note);
private:
  vector<Stream_event*> note_evs_;
  vector<Audio_note*> notes_;
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
      Stream_event *n = note_evs_.back ();
      note_evs_.pop_back ();
      SCM sym = n->get_property ("drum-type");
      SCM defn = SCM_EOL;

      if (scm_is_symbol (sym)
	  && (scm_hash_table_p (tab) == SCM_BOOL_T))
	defn = scm_hashq_ref (tab, sym, SCM_EOL);

      if (Pitch *pit = unsmob_pitch (defn))
	{
          SCM articulations = n->get_property ("articulations");
          Stream_event *tie_event = 0;
          for (SCM s = articulations;
               !tie_event && scm_is_pair (s);
               s = scm_cdr (s))
            {
              Stream_event *ev = unsmob_stream_event (scm_car (s));
              if (!ev)
                continue;
	  
              if (ev->in_event_class ("tie-event"))
                tie_event = ev;
            }

	  Moment len = get_event_length (n, now_mom ());

	  Audio_note *p = new Audio_note (*pit, len,
                                          tie_event, Pitch (0, 0, 0));
	  Audio_element_info info (p, n);
	  announce_element (info);
	  notes_.push_back (p);
	}
    }

  note_evs_.clear ();
}

void
Drum_note_performer::stop_translation_timestep ()
{
  notes_.clear ();
  note_evs_.clear ();
}

IMPLEMENT_TRANSLATOR_LISTENER (Drum_note_performer, note);
void
Drum_note_performer::listen_note (Stream_event *ev)
{
  note_evs_.push_back (ev);
}

ADD_TRANSLATOR (Drum_note_performer,
		/* doc */
		"Play drum notes.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
