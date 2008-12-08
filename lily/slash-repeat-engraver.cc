/*
  slash-repeat-engraver.cc -- implement Slash_repeat_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>, Erik Sandberg
  <mandolaerik@gmail.com>
*/

#include "bar-line.hh"
#include "global-context.hh"
#include "international.hh"
#include "item.hh"
#include "misc.hh"
#include "repeated-music.hh"
#include "score-engraver.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

/**
   This acknowledges repeated music with "percent" style.  It typesets
   a slash sign.
*/
class Slash_repeat_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Slash_repeat_engraver);
protected:
  Stream_event *slash_;
protected:
  DECLARE_TRANSLATOR_LISTENER (percent);
  void process_music ();
};

Slash_repeat_engraver::Slash_repeat_engraver ()
{
  slash_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Slash_repeat_engraver, percent);
void
Slash_repeat_engraver::listen_percent (Stream_event *ev)
{
  /*todo: separate events for percent and slash */
  Moment meas_length
    = robust_scm2moment (get_property ("measureLength"), Moment (0));
  
  if (get_event_length (ev) < meas_length)
    ASSIGN_EVENT_ONCE (slash_, ev);
  
  /*
    don't warn if nothing happens: this can happen if there are whole
    measure repeats.
   */
}

void
Slash_repeat_engraver::process_music ()
{
  if (slash_)
    {
      make_item ("RepeatSlash", slash_->self_scm ());
      slash_ = 0;
    }
}

ADD_TRANSLATOR (Slash_repeat_engraver,
		/* doc */
		"Make beat repeats.",

		/* create */
		"RepeatSlash ",

		/* read */
		"measureLength ",

		/* write */
		""
		);
