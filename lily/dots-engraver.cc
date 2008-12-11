/* 
  dots-engraver.cc -- implement Dots_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2006--2008 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "engraver.hh"
#include "duration.hh"
#include "item.hh"
#include "rhythmic-head.hh"
#include "stream-event.hh"

#include "translator.icc"


class Dots_engraver : public Engraver 
{
  DECLARE_ACKNOWLEDGER (rhythmic_head);
  TRANSLATOR_DECLARATIONS (Dots_engraver);
};

Dots_engraver::Dots_engraver ()
{
}

void
Dots_engraver::acknowledge_rhythmic_head (Grob_info gi)
{
  Stream_event *cause = gi.event_cause ();
  if (!cause)
    return;

  Grob *note = gi.grob ();
  if (unsmob_grob (note->get_object ("dot")))
    return;
  
  Duration *dur = unsmob_duration (cause->get_property ("duration"));
  if (dur && dur->dot_count ())
    {
      Item *d = make_item ("Dots", note->self_scm ());
      Rhythmic_head::set_dots (note, d);

      d->set_property ("dot-count", scm_from_int (dur->dot_count ()));
      d->set_parent (note, Y_AXIS);
    }
}


ADD_ACKNOWLEDGER (Dots_engraver, rhythmic_head);

ADD_TRANSLATOR (Dots_engraver,
		"Create @ref{Dots} objects for"
		" @ref{rhythmic-head-interface}s.",

		/* create */
		"Dots ",

		/* read */
		"",
	       
		/* write */
		""
		);
