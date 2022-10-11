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

#include "config.hh"

#include "audio-item.hh"
#include "input.hh"
#include "international.hh"
#include "libc-extension.hh"
#include "midi-cc-announcer.hh"

#include <cmath>

using std::string;

/*
  Context properties for setting MIDI controls.  Each MIDI control
  specification has the following components:
    1. The name of the LilyPond context property used to change the value of
       the MIDI control.
    2. The lower bound for the numeric range of the LilyPond context property.
    3. The upper bound for the numeric range of the LilyPond context property.
    4. The MIDI control number for setting the most significant 7 bits of the
       control value.
    5. The MIDI control number for setting the least significant 7 bits of the
       control value, if the control supports 14-bit ("fine") resolution.  If
       the control supports only 7-bit ("coarse") resolution, the LSB control
       number should be negative.
*/
const Midi_control_change_announcer::Control_spec
  Midi_control_change_announcer::controls_[]
  = {{"midiBalance", -1.0, 1.0, 8, 40},
     {"midiPanPosition", -1.0, 1.0, 10, 42},
     {"midiExpression", 0.0, 1.0, 11, 43},
     {"midiReverbLevel", 0.0, 1.0, 91, -1},
     {"midiChorusLevel", 0.0, 1.0, 93, -1},
     // This element should be kept last in the array.
     {0, 0.0, 0.0, 0, 0}};

Midi_control_change_announcer::Midi_control_change_announcer (Input *origin)
  : origin_ (origin)
{
}

Midi_control_change_announcer::~Midi_control_change_announcer ()
{
}

void
Midi_control_change_announcer::announce_control_changes ()
{
  for (const Control_spec *spec = controls_; spec->context_property_name_;
       ++spec)
    {
      SCM value = get_property_value (spec->context_property_name_);
      if (!scm_is_number (value))
        continue;
      Real val = from_scm<double> (value);
      if (val >= spec->range_min_ && val <= spec->range_max_)
        {
          // Normalize the value to the 0.0 to 1.0 range.
          val = ((val - spec->range_min_)
                 / (spec->range_max_ - spec->range_min_));
          // Transform the normalized context property value into a 14-bit or
          // a 7-bit (non-negative) integer depending on the MIDI control's
          // resolution.  For directional value changes, #CENTER will
          // correspond to 0.5 exactly, and round_halfway_up rounds upwards in
          // case of doubt.  That means that center position will round to
          // 0x40 or 0x2000 by a hair's breadth.
          const Real full_fine_scale = 0x3FFF;
          const Real full_coarse_scale = 0x7F;
          const bool fine_resolution = (spec->lsb_control_number_ >= 0);
          const auto v = static_cast<int> (round_halfway_up (
            val * (fine_resolution ? full_fine_scale : full_coarse_scale)));
          // Announce a control change for the most significant 7 bits of the
          // control value (and, if the control supports fine resolution, for
          // the least significant 7 bits as well).
          do_announce (new Audio_control_change (
            spec->msb_control_number_, fine_resolution ? (v >> 7) : v));
          if (fine_resolution)
            do_announce (
              new Audio_control_change (spec->lsb_control_number_, v & 0x7F));
        }
      else
        warn (_f ("ignoring out-of-range value change for MIDI property `%s'",
                  spec->context_property_name_));
    }
}

void
Midi_control_change_announcer::warn (const string &message)
{
  if (origin_)
    origin_->warning (message);
  else
    warning (message);
}
