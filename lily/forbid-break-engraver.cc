#include "rhythmic-head.hh"
#include "engraver.hh"
#include "grob.hh"
#include "score-engraver.hh"

class Forbid_line_break_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Forbid_line_break_engraver);
  virtual void start_translation_timestep ();
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
  while (ly_pair_p (busy) && unsmob_moment (ly_caar (busy))->main_part_ == now.main_part_)
    busy = ly_cdr (busy);

  
  while (ly_pair_p (busy))
    {
      Grob *g = unsmob_grob (ly_cdar (busy));
      if (Rhythmic_head::has_interface (g))
	{
	  get_score_engraver ()->forbid_breaks ();
	}
      busy = ly_cdr (busy);
    }
}


ENTER_DESCRIPTION (Forbid_line_break_engraver,
/* descr */       "Forbid line breaks when note heads are still playing at some point.",
/* creats*/       "",
/* accepts */     "",
/* acks  */      "",
/* reads */       "busyGrobs",
/* write */       "");
