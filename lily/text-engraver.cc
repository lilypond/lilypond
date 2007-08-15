/*
  text-engraver.cc -- implement Text_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "directional-element-interface.hh"
#include "engraver.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "text-interface.hh"

#include "translator.icc"

/**
   typeset directions that are  plain text.
*/
class Text_engraver : public Engraver
{
  vector<Stream_event *> evs_;
  vector<Item*> texts_;
public:
  TRANSLATOR_DECLARATIONS (Text_engraver);
protected:
  void stop_translation_timestep ();
  void process_acknowledged ();

  DECLARE_TRANSLATOR_LISTENER (text_script);
  DECLARE_ACKNOWLEDGER (stem_tremolo);
  DECLARE_ACKNOWLEDGER (stem);
  DECLARE_ACKNOWLEDGER (rhythmic_head);
};

IMPLEMENT_TRANSLATOR_LISTENER (Text_engraver, text_script);
void
Text_engraver::listen_text_script (Stream_event *ev)
{
  evs_.push_back (ev);
}

void
Text_engraver::acknowledge_rhythmic_head (Grob_info inf)
{
  for (vsize i = 0; i < texts_.size (); i++)
    {
      Grob *t = texts_[i];
      Side_position_interface::add_support (t, inf.grob ());

      /*
	ugh.
      */
      if (Side_position_interface::get_axis (t) == X_AXIS
	  && !t->get_parent (Y_AXIS))
	t->set_parent (inf.grob (), Y_AXIS);
      else if (Side_position_interface::get_axis (t) == Y_AXIS
	       && !t->get_parent (X_AXIS))
	t->set_parent (inf.grob (), X_AXIS);
    }
}

void
Text_engraver::acknowledge_stem (Grob_info inf)
{
  for (vsize i = 0; i < texts_.size (); i++)
    Side_position_interface::add_support (texts_[i], inf.grob ());
}

void
Text_engraver::acknowledge_stem_tremolo (Grob_info info)
{
  for (vsize i = 0; i < texts_.size (); i++)
    Side_position_interface::add_support (texts_[i], info.grob ());
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

ADD_ACKNOWLEDGER (Text_engraver, stem);
ADD_ACKNOWLEDGER (Text_engraver, stem_tremolo);
ADD_ACKNOWLEDGER (Text_engraver, rhythmic_head);
ADD_TRANSLATOR (Text_engraver,
		/* doc */ "Create text-scripts",
		/* create */ "TextScript",
		/* read */ "",
		/* write */ "");
