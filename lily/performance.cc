/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "performance.hh"

#include <ctime>
using namespace std;

#include "audio-column.hh"
#include "audio-staff.hh"
#include "file-name.hh"
#include "international.hh"
#include "lily-version.hh"
#include "main.hh"
#include "midi-chunk.hh"
#include "midi-stream.hh"
#include "score.hh"
#include "string-convert.hh"
#include "warn.hh"

Performance::Performance ()
{
  midi_ = 0;
}

Performance::~Performance ()
{
  junk_pointers (audio_elements_);
}

void
Performance::output (Midi_stream &midi_stream) const
{
  int tracks_ = audio_staffs_.size ();

  midi_stream.write (Midi_header (1, tracks_, 384));
  if (be_verbose_global)
    progress_indication (_ ("Track...") + " ");
  
  int channel = 0;
  for (vsize i = 0; i < audio_staffs_.size (); i++)
    {
      Audio_staff *s = audio_staffs_[i];
      if (be_verbose_global)
	progress_indication ("[" + to_string (i));

      int midi_channel =  s->channel_;

      if (midi_channel < 0)
	{
	  midi_channel = channel;
	  channel ++;
	  /*
	    MIDI players tend to ignore instrument settings on
	    channel 10, the percussion channel.
	  */
	  if (channel % 16 == 9)
	    channel ++;
	}

      /*
	Huh? Why does each staff also have a separate channel? We
	should map channels to voices, not staves. --hwn.
      */
      if (midi_channel > 15)
	{
	  warning (_ ("MIDI channel wrapped around"));
	  warning (_ ("remapping modulo 16"));

	  midi_channel = midi_channel % 16; 
	}

      s->output (midi_stream, midi_channel);
      if (be_verbose_global)
	progress_indication ("]");
    }
}
void
Performance::add_element (Audio_element *p)
{
  audio_elements_.push_back (p);
}

void
Performance::write_output (string out) const
{
  if (out == "-")
    out = "lelie.midi";

  /* Maybe a bit crude, but we had this before */
  File_name file_name (out);
  out = file_name.to_string ();

  Midi_stream midi_stream (out);
  message (_f ("MIDI output to `%s'...", out));

  output (midi_stream);
  progress_indication ("\n");
}


void
Performance::process ()
{
}

Performance *
unsmob_performance (SCM x)
{
  return dynamic_cast<Performance*> (unsmob_music_output (x));
}
