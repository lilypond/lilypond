/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2016--2022 by Heikki Tauriainen <g034737@welho.com>.

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

#ifndef MIDI_CC_ANNOUNCER_HH
#define MIDI_CC_ANNOUNCER_HH

#include "input.hh"
#include "performer.hh"
#include "audio-item.hh"

/* Base class for announcing MIDI control changes. */
class Midi_control_change_announcer
{
public:
  /* Constructor.  The optional parameter can be used to specify an Input
     to use for relativizing warning messages about out-of-range values. */
  Midi_control_change_announcer (Input *origin = 0);
  virtual ~Midi_control_change_announcer ();

  void announce_from_context_properties ();

  /* Announces MIDI CC changes by creating new Audio_control_change events
     from them, and calling 'do_announce' on each event.  Control change
     events will be created from every supported MIDI context property for
     which the 'get_property_value' function returns a value that is
     compatible with the expected type of the context property's value. */
  void announce_control_changes ();

private:
  virtual SCM get_property_value (const char *property_name) = 0;
  virtual void do_announce (Audio_control_change *item) = 0;
  void warn (const std::string &message);

  Input *origin_;

  struct Control_spec
  {
    const char *const context_property_name_;
    const Real range_min_;
    const Real range_max_;
    const int msb_control_number_;
    const int lsb_control_number_;
  };

  static const Control_spec controls_[];
};

#endif // MIDI_CC_ANNOUNCER_HH
