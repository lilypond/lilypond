/*
  ligature-bracket-engraver.cc -- implement Ligature_bracket_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2002--2009 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "international.hh"

#include "ligature-engraver.hh"
#include "note-column.hh"
#include "tuplet-bracket.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "spanner.hh"
#include "item.hh"

#include "translator.icc"

class Ligature_bracket_engraver : public Engraver
{
protected:
  virtual void process_music ();
  virtual void stop_translation_timestep ();
  DECLARE_ACKNOWLEDGER (rest);
  DECLARE_ACKNOWLEDGER (note_column);
  DECLARE_TRANSLATOR_LISTENER (ligature);
public:
  TRANSLATOR_DECLARATIONS (Ligature_bracket_engraver);

private:
  Drul_array<Stream_event *> events_drul_;
  Spanner *finished_ligature_;
  Spanner *ligature_;
  Stream_event *previous_start_event_;
};

IMPLEMENT_TRANSLATOR_LISTENER (Ligature_bracket_engraver, ligature);
void
Ligature_bracket_engraver::listen_ligature (Stream_event *ev)
{
  Direction d = to_dir (ev->get_property ("span-direction"));
  ASSIGN_EVENT_ONCE (events_drul_[d], ev);
}

Ligature_bracket_engraver::Ligature_bracket_engraver ()
{
  ligature_ = 0;
  finished_ligature_ = 0;
  events_drul_[LEFT] = events_drul_[RIGHT] = 0;
  previous_start_event_ = 0;
}

void
Ligature_bracket_engraver::process_music()
{
  if (events_drul_[STOP])
    {
      if (!ligature_)
	{
	  events_drul_[STOP]->origin ()->warning (_ ("cannot find start of ligature"));
	  return;
	}

      finished_ligature_ = ligature_;
      ligature_ = 0;
      previous_start_event_ = 0;
    }

  if (events_drul_[START])
    {
      if (ligature_)
	{
	  events_drul_[START]->origin ()->warning (_ ("already have a ligature"));
	  return;
	}

      previous_start_event_ = events_drul_[START];
      ligature_ = make_spanner ("LigatureBracket", events_drul_[START]->self_scm ());
    }
}

void
Ligature_bracket_engraver::acknowledge_note_column (Grob_info info)
{
  if (ligature_)
    {
      Tuplet_bracket::add_column (ligature_,
				  info.item ());
      add_bound_item (ligature_, info.item());
    }
}

void
Ligature_bracket_engraver::acknowledge_rest (Grob_info info)
{
  acknowledge_note_column(info);
}


void
Ligature_bracket_engraver::stop_translation_timestep ()
{
  events_drul_[LEFT] =  
    events_drul_[RIGHT] = 0;
  finished_ligature_ = 0;
}

ADD_ACKNOWLEDGER (Ligature_bracket_engraver, rest);
ADD_ACKNOWLEDGER (Ligature_bracket_engraver, note_column);

ADD_TRANSLATOR (Ligature_bracket_engraver,
		/* doc */
		"Handle @code{Ligature_events} by engraving @code{Ligature}"
		" brackets.",

		/* create */
		"LigatureBracket ",

		/* read */
		"",

		/* write */
		""
		);
