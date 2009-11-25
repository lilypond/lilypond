/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "engraver.hh"
#include "context.hh"
#include "grob.hh"
#include "stream-event.hh"

#include "translator.icc"


class Output_property_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Output_property_engraver);
protected:
  vector<Stream_event*> props_;
  
  DECLARE_ACKNOWLEDGER (grob);
  DECLARE_TRANSLATOR_LISTENER (apply_output);

  void stop_translation_timestep ();
};

IMPLEMENT_TRANSLATOR_LISTENER (Output_property_engraver, apply_output);
void
Output_property_engraver::listen_apply_output (Stream_event *ev)
{
  /*
    UGH. Only swallow the output property event in the context
    it was intended for. This is inelegant but not inefficient.
  */
  if (context ()->is_alias (ev->get_property ("context-type")))
    props_.push_back (ev);
}

void
Output_property_engraver::acknowledge_grob (Grob_info inf)
{
  for (vsize i = props_.size (); i--;)
    {
      Stream_event *o = props_[i];
      Context *d = inf.context ();
      SCM proc = o->get_property ("procedure");
      scm_call_3 (proc,
		  inf.grob ()->self_scm (),
		  d->self_scm (), 
		  context ()->self_scm ());
    }
}

void
Output_property_engraver::stop_translation_timestep ()
{
  props_.clear ();
}

Output_property_engraver::Output_property_engraver ()
{
}

ADD_ACKNOWLEDGER (Output_property_engraver, grob);
ADD_TRANSLATOR (Output_property_engraver,
		/* doc */
		"Apply a procedure to any grob acknowledged.",
		
		/* create */
		"",
		
		/* read */
		"",
		
		/* write */
		""
		);
