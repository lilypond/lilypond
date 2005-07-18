/*
  forbid-break-engraver.cc -- implement Forbid_line_break_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2002--_2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/
#include "rhythmic-head.hh"
#include "grob.hh"
#include "score-engraver.hh"
#include "input.hh"
#include "pitch.hh"
#include "duration.hh"
#include "moment.hh"

class Forbid_line_break_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Forbid_line_break_engraver);
  PRECOMPUTED_VIRTUAL void start_translation_timestep ();
};

Forbid_line_break_engraver::Forbid_line_break_engraver (){}

void
Forbid_line_break_engraver::start_translation_timestep ()
{
  /*
    Check for running note heads. This should probably be done elsewhere.
  */
  SCM busy = get_property ("busyGrobs");

  Moment now = now_mom ();
  while (scm_is_pair (busy) && unsmob_moment (scm_caar (busy))->main_part_ == now.main_part_)
    busy = scm_cdr (busy);

  while (scm_is_pair (busy))
    {
      Grob *g = unsmob_grob (scm_cdar (busy));
      if (Rhythmic_head::has_interface (g))
	{
	  get_score_engraver ()->forbid_breaks ();
	}
      busy = scm_cdr (busy);
    }
}

#include "translator.icc"

ADD_TRANSLATOR (Forbid_line_break_engraver,
		/* descr */ "Forbid line breaks when note heads are still playing at some point.",
		/* creats*/ "",
		/* accepts */ "",
		/* acks  */ "",
		/* reads */ "busyGrobs",
		/* write */ "");
