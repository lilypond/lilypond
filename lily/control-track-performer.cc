/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "audio-item.hh"
#include "audio-staff.hh"
#include "lily-version.hh"
#include "performer.hh"
#include "string-convert.hh"
#include "warn.hh"

#include "translator.icc"

using std::string;

class Control_track_performer : public Performer
{
  Audio_staff *control_track_;

  void add_text (Audio_text::Type, const string &);
  TRANSLATOR_DECLARATIONS (Control_track_performer);

protected:
  void initialize () override;
  void acknowledge_audio_element (Audio_element_info info) override;
  void finalize () override;
};

Control_track_performer::Control_track_performer (Context *c)
  : Performer (c)
{
  control_track_ = 0;
}

void
Control_track_performer::acknowledge_audio_element (Audio_element_info info)
{
  if (auto *tempo = dynamic_cast<Audio_tempo *> (info.elem_))
    {
      control_track_->add_audio_item (tempo);
    }
  else if (auto *text = dynamic_cast<Audio_text *> (info.elem_))
    {
      if (text->type_ == Audio_text::MARKER)
        control_track_->add_audio_item (text);
    }
  else if (auto *sig = dynamic_cast<Audio_time_signature *> (info.elem_))
    {
      control_track_->add_audio_item (sig);
    }
}

void
Control_track_performer::add_text (Audio_text::Type text_type,
                                   const string &str)
{
  Audio_item *text = new Audio_text (text_type, str);
  control_track_->add_audio_item (text);

  announce_element (Audio_element_info (text, 0));
}

void
Control_track_performer::initialize ()
{
  control_track_ = new Audio_control_track_staff;
  announce_element (Audio_element_info (control_track_, 0));

  string id_string
    = String_convert::pad_to ("LilyPond " + version_string (), 30);

  // The first audio element in the control track is a placeholder for the
  // name of the MIDI sequence.  The actual name is stored in the element
  // later before outputting the track (in Performance::output, see
  // performance.cc).
  add_text (Audio_text::TRACK_NAME, "control track");
  add_text (Audio_text::TEXT, "creator: ");
  add_text (Audio_text::TEXT, id_string);
}

void
Control_track_performer::finalize ()
{
  control_track_->end_mom_
    = now_mom () + from_scm (get_property (this, "midiSkipOffset"), Moment ());
}

void
Control_track_performer::boot ()
{
}

ADD_TRANSLATOR (Control_track_performer,
                /* doc */
                R"(

                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
midiSkipOffset
                )",

                /* write */
                R"(

                )");
