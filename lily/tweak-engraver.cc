/*
  tweak-engraver.cc -- implement Tweak_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

*/

#include "engraver.hh"

#include "music.hh"
#include "grob.hh"
#include "translator.icc"

class Tweak_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Tweak_engraver);

protected:
  DECLARE_ACKNOWLEDGER (grob);
};

Tweak_engraver::Tweak_engraver()
{
}

void
Tweak_engraver::acknowledge_grob (Grob_info info)
{
  if (Music *music = info.music_cause ())
    {
      for (SCM s = music->get_property ("tweaks");
	   scm_is_pair (s); s = scm_cdr (s))
	{
	  info.grob ()->internal_set_property (scm_caar (s), scm_cdar (s));
	}
    }
}

ADD_ACKNOWLEDGER (Tweak_engraver, grob);
ADD_TRANSLATOR (Tweak_engraver,
		/* doc */ "Read the @code{tweaks} property from the originating Music event, and set properties." ,
		
		/* create */ "",
		/* accept */ "",
		/* read */ "",
		/* write */ "");
