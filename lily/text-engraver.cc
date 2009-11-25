/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
