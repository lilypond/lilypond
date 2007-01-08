/*
  melisma-engraver.cc -- implement Melisma_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "grob.hh"
#include "context.hh"
#include "music.hh"
#include "translator.icc"

/* Remove this translator. */

/**
   Signal existence of melismas.
*/
class Melisma_translator : public Translator
{
public:
  TRANSLATOR_DECLARATIONS (Melisma_translator);
protected:
  virtual bool try_music (Music *);
  void process_music ();
  void start_translation_timestep ();
  Music *event_;
};

bool
Melisma_translator::try_music (Music *m)
{
  if (m->is_mus_type ("melisma-playing-event"))
    return melisma_busy (context ());
  else if (m->is_mus_type ("melisma-span-event"))
    {
      event_ = m;
      return true;
    }

  return false;
}

void
Melisma_translator::process_music ()
{
  if (event_)
    {
      SCM sd = event_->get_property ("span-direction");
      Direction d = to_dir (sd);
      if (d == START)
	context ()->set_property ("melismaBusy", SCM_BOOL_T);
      else
	context ()->unset_property (ly_symbol2scm ("melismaBusy"));
    }
}

void
Melisma_translator::start_translation_timestep ()
{
  event_ = 0;
}

Melisma_translator::Melisma_translator ()
{
  event_ = 0;
}

ADD_TRANSLATOR (Melisma_translator,
		/* doc */ "This translator collects melisma information about ties, beams, and user settings (@code{melismaBusy}, and signals it to the @code{\addlyrics} code.  ",
		/* create */ "",
		/* read */
		"beamMelismaBusy "
		"melismaBusy "
		"melismaBusyProperties "
		"slurMelismaBusy "
		"tieMelismaBusy "
		,

		/* write */ "");
