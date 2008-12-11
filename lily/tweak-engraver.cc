/*
  tweak-engraver.cc -- implement Tweak_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "engraver.hh"

#include "grob.hh"
#include "stream-event.hh"
#include "translator.icc"

class Tweak_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Tweak_engraver);

protected:
  DECLARE_ACKNOWLEDGER (grob);
};

Tweak_engraver::Tweak_engraver ()
{
}

void
Tweak_engraver::acknowledge_grob (Grob_info info)
{
  if (Stream_event *ev = info.event_cause ())
    {
      for (SCM s = ev->get_property ("tweaks");
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  info.grob ()->set_property (scm_caar (s), scm_cdar (s));
	}
    }
}

ADD_ACKNOWLEDGER (Tweak_engraver, grob);
ADD_TRANSLATOR (Tweak_engraver,
		/* doc */
		"Read the @code{tweaks} property from the originating event,"
		" and set properties.",
		
		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
