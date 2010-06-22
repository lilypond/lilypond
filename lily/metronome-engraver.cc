/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2010 Jan Nieuwenhuizen <janneke@gnu.org>

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
public:
  TRANSLATOR_DECLARATIONS (Metronome_mark_engraver);

protected:
  Item *text_;
  Grob *support_;
  Grob *bar_;

  SCM last_duration_;
  SCM last_count_;
  SCM last_text_;

  DECLARE_ACKNOWLEDGER (break_aligned);
  DECLARE_ACKNOWLEDGER (grob);

protected:
  virtual void derived_mark () const;
  void stop_translation_timestep ();
  void process_music ();
};

Metronome_mark_engraver::Metronome_mark_engraver ()
{
  text_ = 0;
  support_ = 0;
  bar_ = 0;
  last_duration_ = SCM_EOL;
  last_count_ = SCM_EOL;
  last_text_ = SCM_EOL;
}

void
Metronome_mark_engraver::derived_mark () const
{
  scm_gc_mark (last_count_);
  scm_gc_mark (last_duration_);
  scm_gc_mark (last_text_);
}

void
Metronome_mark_engraver::acknowledge_break_aligned (Grob_info info)
{
  Grob *g = info.grob ();

  if (text_
      && g->get_property_data ("break-align-symbol")
      == ly_symbol2scm ("staff-bar"))
      bar_ = g;
  else if (text_
	   && !support_
	   && scm_member (g->get_property_data ("break-align-symbol"),
			  text_->get_property_data ("break-align-symbols"))
	   != SCM_BOOL_F)
    {
      support_ = g;
      text_->set_parent (g, X_AXIS);
    }
}

SCM
grob_name_scm (Grob *g)
{
  SCM name_pair = scm_assq (ly_symbol2scm ("name"), g->get_property ("meta"));
  return (scm_is_pair (name_pair)
	  ? ly_camel_case_2_lisp_identifier (scm_cdr (name_pair))
	  : SCM_EOL);
}

void
Metronome_mark_engraver::acknowledge_grob (Grob_info info)
{
  Grob *g = info.grob ();

  if (text_
      && scm_member (grob_name_scm (g),
		     text_->get_property_data ("non-break-align-symbols"))
      != SCM_BOOL_F)
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
    }
}

void
Metronome_mark_engraver::process_music ()
{
  SCM count = get_property ("tempoUnitCount");
  SCM duration = get_property ("tempoUnitDuration");
  SCM text = get_property ("tempoText");

  if ( ( (unsmob_duration (duration) && scm_is_number (count))
        || Text_interface::is_markup (text) )
      && !(ly_is_equal (count, last_count_)
	   && ly_is_equal (duration, last_duration_)
	   && ly_is_equal (text, last_text_)))
    {
      text_ = make_item ("MetronomeMark", SCM_EOL);

      SCM proc = get_property ("metronomeMarkFormatter");
      SCM result = scm_call_4 (proc,
			       text,
			       duration,
			       count,
			       context ()->self_scm ());

      text_->set_property ("text", result);
    }

  last_duration_ = duration;
  last_count_ = count;
  last_text_ = text;
}



ADD_ACKNOWLEDGER (Metronome_mark_engraver, break_aligned);
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
		"stavesFound "
		"metronomeMarkFormatter "
		"tempoUnitDuration "
		"tempoUnitCount "
		"tempoText "
		"tempoHideNote ",

		/* write */
		""
		);
