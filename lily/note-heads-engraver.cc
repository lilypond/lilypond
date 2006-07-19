/*
  note-heads-engraver.cc -- part of GNU LilyPond

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"

#include <cctype>
using namespace std;

#include "rhythmic-head.hh"
#include "output-def.hh"
#include "dots.hh"
#include "dot-column.hh"
#include "staff-symbol-referencer.hh"
#include "item.hh"
#include "warn.hh"
#include "duration.hh"

class Note_heads_engraver : public Engraver
{
  vector<Item*> notes_;
  vector<Item*> dots_;
  vector<Music*> note_evs_;

public:
  TRANSLATOR_DECLARATIONS (Note_heads_engraver);

protected:
  virtual bool try_music (Music *ev);
  void process_music ();
  void stop_translation_timestep ();
};

Note_heads_engraver::Note_heads_engraver ()
{
}

bool
Note_heads_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("note-event"))
    {
      note_evs_.push_back (m);
      return true;
    }

  return false;
}

void
Note_heads_engraver::process_music ()
{
  for (vsize i = 0; i < note_evs_.size (); i++)
    {
      Music *ev = note_evs_[i];
      Item *note = make_item ("NoteHead", ev->self_scm ());

      Duration dur = *unsmob_duration (ev->get_property ("duration"));

      note->set_property ("duration-log", scm_from_int (dur.duration_log ()));
      if (dur.dot_count ())
	{
	  Item *d = make_item ("Dots", note->self_scm ());
	  Rhythmic_head::set_dots (note, d);

	  if (dur.dot_count ()
	      != robust_scm2int (d->get_property ("dot-count"), 0))
	    d->set_property ("dot-count", scm_from_int (dur.dot_count ()));

	  d->set_parent (note, Y_AXIS);

	  dots_.push_back (d);
	}

      Pitch *pit = unsmob_pitch (ev->get_property ("pitch"));

#if 0 /* TODO: should have a mechanism to switch off these warnings. */

      if (!pit)
	ev->origin ()->warning (_ ("NoteEvent without pitch"));
#endif

      int pos = pit ? pit->steps () : 0;
      SCM c0 = get_property ("middleCPosition");
      if (scm_is_number (c0))
	pos += scm_to_int (c0);

      note->set_property ("staff-position", scm_from_int (pos));

      /*
	Shape note heads change on step of the scale.
      */
      SCM shape_vector = get_property ("shapeNoteStyles");
      if (scm_is_vector (shape_vector))
	{
	  SCM scm_tonic = get_property ("tonic");
	  Pitch tonic (0, 0, 0);
	  if (unsmob_pitch (scm_tonic))
	    tonic = *unsmob_pitch (scm_tonic);

	  unsigned int delta = (pit->get_notename () - tonic.get_notename () + 7) % 7;

	  SCM style = SCM_EOL;
	  if (scm_c_vector_length (shape_vector) > delta
	      && scm_is_symbol (scm_vector_ref (shape_vector, scm_from_int (delta))))
	    style = scm_vector_ref (shape_vector, scm_from_int (delta));
	  if (scm_is_symbol (style))
	    note->set_property ("style", style);
	}

      notes_.push_back (note);
    }
}

void
Note_heads_engraver::stop_translation_timestep ()
{
  notes_.clear ();
  dots_.clear ();
  note_evs_.clear ();
}

#include "translator.icc"

ADD_TRANSLATOR (Note_heads_engraver,
		/* doc */ "Generate noteheads.",
		/* create */
		"NoteHead "
		"Dots",
		/* accept */
		"note-event",
		/* read */ "middleCPosition",
		/* write */ "");
