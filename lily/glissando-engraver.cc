/*
  note-head-line-engraver.cc -- implement Note_head_line_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "engraver.hh"

#include "international.hh"
#include "rhythmic-head.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "warn.hh"
#include "item.hh"

#include "translator.icc"

class Glissando_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Glissando_engraver);

protected:
  DECLARE_TRANSLATOR_LISTENER (glissando);
  DECLARE_ACKNOWLEDGER (rhythmic_head);
  virtual void finalize ();

  void stop_translation_timestep ();
  void process_music ();
private:
  Spanner *line_;
  Spanner *last_line_;
  Stream_event *event_;
};

Glissando_engraver::Glissando_engraver ()
{
  last_line_ = line_ = 0;
  event_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Glissando_engraver, glissando);
void
Glissando_engraver::listen_glissando (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (event_, ev);
}

void
Glissando_engraver::process_music ()
{
  if (event_)
    line_ = make_spanner ("Glissando", event_->self_scm ());
}

void
Glissando_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  Grob *g = info.grob ();
  if (line_)
    line_->set_bound (LEFT, g);

  if (last_line_)
    {
      last_line_->set_bound (RIGHT, g);
      announce_end_grob (last_line_, g->self_scm ());
    }      
}

void
Glissando_engraver::stop_translation_timestep ()
{
  if (last_line_ && last_line_->get_bound (RIGHT))
    {
      last_line_ = 0;
    }
  if (line_)
    {
      if (last_line_)
	programming_error ("overwriting glissando");
      last_line_ = line_;
    }
  line_ = 0;
  event_ = 0;
}

void
Glissando_engraver::finalize ()
{
  if (line_)
    {
      string msg = _ ("unterminated glissando");

      if (event_)
	event_->origin ()->warning (msg);
      else
	warning (msg);

      line_->suicide ();
      line_ = 0;
    }
}

ADD_ACKNOWLEDGER (Glissando_engraver, rhythmic_head);
ADD_TRANSLATOR (Glissando_engraver,
		/* doc */
		"Engrave glissandi.",

		/* create */
		"Glissando ",

		/* read */
		"",

		/* write */
		""
		);
