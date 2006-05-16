/*
  slash-repeat-engraver.cc -- implement Slash_repeat_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>, Erik Sandberg <mandolaerik@gmail.com>
*/

#include "repeated-music.hh"
#include "global-context.hh"
#include "warn.hh"
#include "misc.hh"
#include "spanner.hh"
#include "item.hh"
#include "percent-repeat-iterator.hh"
#include "bar-line.hh"
#include "score-engraver.hh"

/**
   This acknowledges repeated music with "percent" style.  It typesets
   a slash sign.
*/
class Slash_repeat_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Slash_repeat_engraver);
protected:
  Music *slash_;
protected:
  virtual bool try_music (Music *);
  void process_music ();
};

Slash_repeat_engraver::Slash_repeat_engraver ()
{
  slash_ = 0;
}

bool
Slash_repeat_engraver::try_music (Music *m)
{
  /*todo: separate events for percent and slash */
  if (m->is_mus_type ("percent-event"))
    {
      Moment meas_length
        = robust_scm2moment (get_property ("measureLength"), Moment (0));

      if (m->get_length () < meas_length)
	slash_ = m;
      else
	return false;

      return true;
    }

  return false;
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

#include "translator.icc"

ADD_TRANSLATOR (Slash_repeat_engraver,
		/* doc */ "Make beat repeats.",
		/* create */ "RepeatSlash",
		/* accept */ "percent-event",
		/* read */ "measureLength",
		/* write */ "");
