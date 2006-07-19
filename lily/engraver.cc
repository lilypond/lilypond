/*
  engraver.cc -- implement Engraver

  Sourcefile of GNU LilyPond music type setter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"

#include "context.hh"
#include "item.hh"
#include "lilypond-key.hh"
#include "music.hh"
#include "score-engraver.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

Engraver_group *
Engraver::get_daddy_engraver () const
{
  return dynamic_cast<Engraver_group *> (get_daddy_translator ());
}

void
Engraver::announce_grob (Grob_info inf)
{
  get_daddy_engraver ()->announce_grob (inf);
}

void
Engraver::announce_end_grob (Grob_info inf)
{
  get_daddy_engraver ()->announce_grob (inf);
}

/*
  CAUSE is the object (typically a Stream_event object)  that
  was the reason for making E.
*/
void
Engraver::announce_grob (Grob *e, SCM cause)
{
  /* TODO: Remove Music code when it's no longer needed */
  if (Music *m = unsmob_music (cause))
    {
      cause = m->to_event ()->unprotect ();
    }
  if (unsmob_stream_event (cause) || unsmob_grob (cause))
    e->set_property ("cause", cause);

  Grob_info i (this, e);

  Engraver_group *g = get_daddy_engraver ();
  if (g)
    g->announce_grob (i);
}


/*
  CAUSE is the object (typically a Music object)  that
  was the reason for making E.
*/
void
Engraver::announce_end_grob (Grob *e, SCM cause)
{
  /* TODO: Remove Music code when it's no longer needed */
  if (Music *m = unsmob_music (cause))
    {
      cause = m->to_event ()->unprotect ();
    }
  if (unsmob_stream_event (cause) || unsmob_grob (cause))
    e->set_property ("cause", cause);

  Grob_info i (this, e);

  i.start_end_ = STOP;
  Engraver_group *g = get_daddy_engraver ();
  if (g)
    g->announce_grob (i);
}


Engraver::Engraver ()
{
}

#include "translator.icc"

ADD_TRANSLATOR (Engraver,
		"Base class for engravers. Does nothing, so it is not used.",
		"",
		"",
		"",
		"");

