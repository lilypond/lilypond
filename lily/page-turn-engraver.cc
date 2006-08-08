/*
  page-turn-engraver.cc -- implement Page_turn_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2006 Joe Neeman <joeneeman@gmail.com>
*/

#include "engraver.hh"

#include "context.hh"
#include "duration.hh"
#include "grob.hh"
#include "international.hh"
#include "moment.hh"
#include "warn.hh"

class Page_turn_engraver : public Engraver
{
  Moment rest_begin_;
  Moment repeat_begin_;
  Moment note_end_;
  Rational repeat_begin_rest_length_;

  Real penalty (Rational rest_len);

protected:
  DECLARE_ACKNOWLEDGER (note_head);

public:
  TRANSLATOR_DECLARATIONS (Page_turn_engraver);
  void stop_translation_timestep ();
};

Page_turn_engraver::Page_turn_engraver ()
{
  repeat_begin_ = Moment (-1);
  repeat_begin_rest_length_ = 0;
  rest_begin_ = 0;
  note_end_ = 0;
}

Real
Page_turn_engraver::penalty (Rational rest_len)
{
  Rational min_turn = robust_scm2moment (get_property ("minPageTurnLength"), Moment (1)).main_part_;

  return (rest_len < min_turn) ? infinity_f : 0;
}

void
Page_turn_engraver::acknowledge_note_head (Grob_info gi)
{
  SCM dur_log_scm = gi.grob ()->get_property ("duration-log");
  if (!scm_is_number (dur_log_scm))
    return;

  int dur_log = scm_to_int (dur_log_scm);
  int dot_count = robust_scm2int (gi.grob ()->get_property ("dot-count"), 0);

  if (rest_begin_ < now_mom ())
    {
      Real pen = penalty ((now_mom () - rest_begin_).main_part_);
      if (!isinf (pen))
	{
          SCM val = scm_cons (rest_begin_.smobbed_copy (), scm_from_double (pen));
          context ()->get_score_context ()->set_property ("allowPageTurn", val);
        }
    }

  if (rest_begin_ <= repeat_begin_)
    repeat_begin_rest_length_ = (now_mom () - repeat_begin_).main_part_;
  note_end_ = now_mom () + Moment (Duration (dur_log, dot_count).get_length ());
}

void
Page_turn_engraver::stop_translation_timestep ()
{
  /* C&P from Repeat_acknowledge_engraver */
  SCM cs = get_property ("repeatCommands");
  bool start = false;
  bool end = false;

  for (; scm_is_pair (cs); cs = scm_cdr (cs))
    {
      SCM command = scm_car (cs);
      if (command == ly_symbol2scm ("start-repeat"))
	start = true;
      else if (command == ly_symbol2scm ("end-repeat"))
	end = true;
    }

  if (end && repeat_begin_.main_part_ >= Moment (0))
    {
      Real pen = penalty ((now_mom () - rest_begin_).main_part_ + repeat_begin_rest_length_);
      Moment *m = unsmob_moment (get_property ("minimumRepeatLengthForPageTurn"));
      if (m && *m > (now_mom () - repeat_begin_))
	pen = infinity_f;
      if (pen > 0)
	{
	  SCM val = scm_cons (repeat_begin_.smobbed_copy (), scm_from_double (pen));
	  context ()->get_score_context ()->set_property ("revokePageTurns", val);
	}
      repeat_begin_ = Moment (-1);
    }
  if (start)
    {
      repeat_begin_ = now_mom ();
      repeat_begin_rest_length_ = 0;
    }
  rest_begin_ = note_end_;
}

#include "translator.icc"

ADD_ACKNOWLEDGER (Page_turn_engraver, note_head);
ADD_TRANSLATOR (Page_turn_engraver,
                /* doc */ "Decide where page turns are allowed to go",
                /* create */ "",
                /* accept */ "",
                /* read */ "",
                /* write */
		"allowPageTurn "
		"revokePageTurns "
		);
