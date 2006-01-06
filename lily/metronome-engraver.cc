/*
  mark-engraver.cc -- implement Metronome_mark_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2006 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <cctype>
using namespace std;

#include "engraver.hh"

#include "note-column.hh"
#include "context.hh"
#include "grob-array.hh"

/**
   put stuff over or next to  bars.  Examples: bar numbers, marginal notes,
   rehearsal marks.
*/
class Metronome_mark_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Metronome_mark_engraver);
protected:
  Item *text_;
  Grob *bar_line_;
  Music *mark_ev_;

  void create_items (Music *);
protected:
  void stop_translation_timestep ();
  virtual bool try_music (Music *ev);
  void process_music ();
};

Metronome_mark_engraver::Metronome_mark_engraver ()
{
  text_ = 0;
  mark_ev_ = 0;
}

void
Metronome_mark_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      Grob *mc = unsmob_grob (get_property ("currentMusicalColumn"));
      text_->set_parent (mc, X_AXIS);
      text_->set_object ("side-support-elements",
			 grob_list_to_grob_array (get_property ("stavesFound")));

      text_ = 0;
    }
  mark_ev_ = 0;
}

void
Metronome_mark_engraver::create_items (Music *rq)
{
  if (text_)
    return;

  text_ = make_item ("MetronomeMark", rq->self_scm ());
}

bool
Metronome_mark_engraver::try_music (Music *r)
{
  mark_ev_ = r;
  return true;
}

void
Metronome_mark_engraver::process_music ()
{
  if (mark_ev_)
    {
      create_items (mark_ev_);

      SCM proc = get_property ("metronomeMarkFormatter");
      SCM result = scm_call_2 (proc, mark_ev_->self_scm (),
			       context ()->self_scm ());

      text_->set_property ("text", result);
    }
}

#include "translator.icc"

ADD_TRANSLATOR (Metronome_mark_engraver,
		/* doc */ "Engrave metro nome marking. This delegates the formatting work "
		"to the function in the metronomeMarkFormatter property. "
		"The mark is put over all staves. "
		"The staves are taken from the @code{stavesFound} property, "
		"which is maintained by @code{@ref{Staff_collecting_engraver}}. ",
		/* create */ "MetronomeMark",
		/* accept */ "metronome-change-event",
		/* read */ "stavesFound metronomeMarkFormatter",
		/* write */ "");
