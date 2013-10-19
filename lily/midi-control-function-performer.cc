/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2013 by Heikki Tauriainen <g034737@welho.com>.
  Adapted from performer implementations
  Copyright (C) 1996--2012 Jan Nieuwenhuizen <janneke@gnu.org>,
  Han-Wen Nienhyus <hanwen@xs4all.nl> and others.

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

#include "performer.hh"

#include "audio-item.hh"
#include "context.hh"
#include "dispatcher.hh"
#include "international.hh"
#include "listener.hh"
#include "stream-event.hh"

#include "translator.icc"

/**
   MIDI control function performer.  Announces "set property" events on MIDI
   context properties.
*/
class Midi_control_function_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Midi_control_function_performer);
  DECLARE_LISTENER (announce_function_value_change);
  ~Midi_control_function_performer ();

  void connect_to_context (Context *c);
  void disconnect_from_context (Context *c);
};

Midi_control_function_performer::Midi_control_function_performer ()
{
}

Midi_control_function_performer::~Midi_control_function_performer ()
{
}

void
Midi_control_function_performer::connect_to_context (Context *c)
{
  c->events_below ()->
    add_listener (GET_LISTENER (announce_function_value_change),
                  ly_symbol2scm ("SetProperty"));
}

void
Midi_control_function_performer::disconnect_from_context (Context *c)
{
  c->events_below ()->
    remove_listener (GET_LISTENER (announce_function_value_change),
                     ly_symbol2scm ("SetProperty"));
}

IMPLEMENT_LISTENER (Midi_control_function_performer,
                    announce_function_value_change)
void
Midi_control_function_performer::announce_function_value_change (SCM sev)
{
  Stream_event *ev = unsmob_stream_event (sev);
  SCM sym = ev->get_property ("symbol");
  if (!scm_is_symbol (sym))
    return;

  // Search for a matching context property; if found, check that the value
  // of the property is within the allowed range, and announce a possible
  // change in the value of the corresponding control function.
  string symbol = ly_symbol2string (sym);
  for (const Audio_control_function_value_change::Context_property *p
         = Audio_control_function_value_change::context_properties_;
       p->name_; ++p)
    {
      if (symbol == p->name_)
        {
          SCM value = ev->get_property ("value");
          if (scm_is_number (value))
            {
              Real val = scm_to_double (value);
              if (val >= p->range_min_ && val <= p->range_max_)
                {
                  // Normalize the value to the 0.0 to 1.0 range.
                  val = ((val - p->range_min_)
                         / (p->range_max_ - p->range_min_));
                  Audio_control_function_value_change *item
                    = new Audio_control_function_value_change (p->control_,
                                                               val);
                  announce_element (Audio_element_info (item, 0));
                }
              else
                ev->origin ()->
                  warning (_f ("ignoring out-of-range value change for MIDI "
                               "property `%s'",
                               p->name_));
            }
          break;
        }
    }
}

ADD_TRANSLATOR (Midi_control_function_performer,
                /* doc */
                "",

                /* create */
                "",

                /* read */
                "midiBalance "
                "midiPanPosition "
                "midiReverbLevel "
                "midiChorusLevel ",

                /* write */
                ""
               );
