/*
  performance.cc -- implement Performance

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Jan Nieuwenhuizen <janneke@gnu.org>
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
#include "midi-item.hh"
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

  // ugh
  int clocks_per_4 = 384;

  midi_stream << Midi_header (1, tracks_, clocks_per_4);
  message (_ ("Track...") + " ");
  
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
  file_name.ext_ = "midi";
  out = file_name.to_string ();

  Midi_stream midi_stream (out);
  message (_f ("MIDI output to `%s'...", out));

  output (midi_stream);
  progress_indication ("\n");
}


void
Performance::remap_grace_durations ()
{
  for (vsize i = 0; i < audio_elements_.size (); i++)
    {
      if (Audio_column * col = dynamic_cast<Audio_column*> (audio_elements_[i]))
	{
	  col->when_.main_part_ = col->when_.main_part_ + Rational (1,4) * col->when_.grace_part_;
	  col->when_.grace_part_ = Rational (0);
	}
    }
}

void
Performance::process ()
{
  remap_grace_durations ();
}
