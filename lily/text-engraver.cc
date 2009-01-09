/*
  text-engraver.cc -- implement Text_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "directional-element-interface.hh"
#include "engraver.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "text-interface.hh"
#include "item.hh"

#include "translator.icc"

/**
   typeset directions that are  plain text.
*/
class Text_engraver : public Engraver
{
  vector<Stream_event *> evs_;
  vector<Grob*> texts_;
public:
  TRANSLATOR_DECLARATIONS (Text_engraver);
protected:
  void stop_translation_timestep ();
  void process_acknowledged ();

  DECLARE_TRANSLATOR_LISTENER (text_script);
};

IMPLEMENT_TRANSLATOR_LISTENER (Text_engraver, text_script);
void
Text_engraver::listen_text_script (Stream_event *ev)
{
  evs_.push_back (ev);
}

void
Text_engraver::process_acknowledged ()
{
  if (texts_.size ())
    return;
  for (vsize i = 0; i < evs_.size (); i++)
    {
      Stream_event *r = evs_[i];

      // URG: Text vs TextScript
      Item *text = make_item ("TextScript", r->self_scm ());

      int priority = robust_scm2int (text->get_property ("script-priority"),
				     200);

      /* see script-engraver.cc */
      priority += i;

      text->set_property ("script-priority", scm_from_int (priority));

      Direction dir = to_dir (r->get_property ("direction"));
      if (dir)
	set_grob_direction (text, dir);

      SCM mark = r->get_property ("text");

      text->set_property ("text", mark);
      texts_.push_back (text);
    }
}

void
Text_engraver::stop_translation_timestep ()
{
  texts_.clear ();
  evs_.clear ();
}

Text_engraver::Text_engraver ()
{
}

ADD_TRANSLATOR (Text_engraver,
		/* doc */
		"Create text scripts.",

		/* create */
		"TextScript ",

		/* read */
		"",

		/* write */
		""
		);
