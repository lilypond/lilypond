/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2011 Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <cctype>
using namespace std;

#include "engraver.hh"

#include "context.hh"
#include "duration.hh"
#include "grob-array.hh"
#include "item.hh"
#include "music.hh"
#include "stream-event.hh"
#include "text-interface.hh"

#include "translator.icc"

class Metronome_mark_engraver : public Engraver
{
  Item *text_;
  Grob *support_;
  Grob *bar_;
  Stream_event *tempo_ev_;

public:
  TRANSLATOR_DECLARATIONS (Metronome_mark_engraver);

protected:
  void stop_translation_timestep ();
  void process_music ();

  DECLARE_ACKNOWLEDGER (break_aligned);
  DECLARE_ACKNOWLEDGER (break_alignment);
  DECLARE_ACKNOWLEDGER (grob);

  DECLARE_TRANSLATOR_LISTENER (tempo_change);
};

Metronome_mark_engraver::Metronome_mark_engraver ()
{
  text_ = 0;
  support_ = 0;
  bar_ = 0;
  tempo_ev_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Metronome_mark_engraver, tempo_change);
void
Metronome_mark_engraver::listen_tempo_change (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (tempo_ev_, ev);
}

static bool
safe_is_member (SCM scm, SCM lst)
{
  return scm_list_p (lst) == SCM_BOOL_T
    && scm_member (scm, lst) != SCM_BOOL_F;
}

void
Metronome_mark_engraver::acknowledge_break_aligned (Grob_info info)
{
  Grob *g = info.grob ();

  if (text_
      && g->get_property ("break-align-symbol")
      == ly_symbol2scm ("staff-bar"))
    bar_ = g;
  else if (text_
	   && !support_
	   && safe_is_member (g->get_property ("break-align-symbol"),
			      text_->get_property ("break-align-symbols"))
	   && Item::break_visible (g))
    {
      support_ = g;
      text_->set_parent (g, X_AXIS);
    }
}

void
Metronome_mark_engraver::acknowledge_break_alignment (Grob_info info)
{
  Grob *g = info.grob ();

  if (text_
      && support_
      && dynamic_cast<Item *> (g))
    text_->set_parent (g, X_AXIS);
}

void
Metronome_mark_engraver::acknowledge_grob (Grob_info info)
{
  Grob *g = info.grob ();

  if (text_)
    for (SCM s = text_->get_property ("non-break-align-symbols");
	 scm_is_pair (s);
	 s = scm_cdr (s))
      if (g->internal_has_interface (scm_car (s)))
	text_->set_parent (g, X_AXIS);
}

void
Metronome_mark_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      if (text_->get_parent (X_AXIS)
	  && text_->get_parent (X_AXIS)->internal_has_interface (ly_symbol2scm ("multi-measure-rest-interface"))
	  && bar_)
	text_->set_parent (bar_, X_AXIS);
      else if (!support_)
	{
	  /*
	    Gardner Read "Music Notation", p.278

	    Align the metronome mark over the time signature (or the
	    first notational element of the measure if no time
	    signature is present in that measure).
	  */
	  if (Grob *mc = unsmob_grob (get_property ("currentMusicalColumn")))
	    text_->set_parent (mc, X_AXIS);
	  else if (Grob *cc = unsmob_grob (get_property ("currentCommandColumn")))
	    text_->set_parent (cc, X_AXIS);
	}
      text_->set_object ("side-support-elements",
			 grob_list_to_grob_array (get_property ("stavesFound")));
      text_ = 0;
      support_ = 0;
      bar_ = 0;
      tempo_ev_ = 0;
    }
}

void
Metronome_mark_engraver::process_music ()
{
  if (tempo_ev_)
    {
      text_ = make_item ("MetronomeMark", tempo_ev_->self_scm ());

      SCM proc = get_property ("metronomeMarkFormatter");
      SCM result = scm_call_2 (proc,
			       tempo_ev_->self_scm (),
			       context ()->self_scm ());

      text_->set_property ("text", result);
    }
}

ADD_ACKNOWLEDGER (Metronome_mark_engraver, break_aligned);
ADD_ACKNOWLEDGER (Metronome_mark_engraver, break_alignment);
ADD_ACKNOWLEDGER (Metronome_mark_engraver, grob);

ADD_TRANSLATOR (Metronome_mark_engraver,
		/* doc */
		"Engrave metronome marking.  This delegates the formatting"
		" work to the function in the @code{metronomeMarkFormatter}"
		" property.  The mark is put over all staves.  The staves are"
		" taken from the @code{stavesFound} property, which is"
		" maintained by @ref{Staff_collecting_engraver}.",

		/* create */
		"MetronomeMark ",

		/* read */
		"currentCommandColumn "
		"currentMusicalColumn "
		"metronomeMarkFormatter "
		"stavesFound "
		"tempoHideNote ",

		/* write */
		""
		);
