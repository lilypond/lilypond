/*
  mark-engraver.cc -- implement Metronome_mark_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <cctype>
using namespace std;

#include "engraver.hh"

#include "context.hh"
#include "duration.hh"
#include "grob-array.hh"
#include "item.hh"
#include "stream-event.hh"
#include "text-interface.hh"

#include "translator.icc"

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

  SCM last_duration_;
  SCM last_count_;
  SCM last_text_;
  
protected:
  virtual void derived_mark () const;
  void stop_translation_timestep ();
  void process_music ();
};

Metronome_mark_engraver::Metronome_mark_engraver ()
{
  text_ = 0;
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
