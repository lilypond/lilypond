/*
  performance.cc -- implement Performance

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <time.h>
#include "debug.hh"
#include "string.hh"
#include "string-convert.hh"
#include "main.hh"
#include "midi-def.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "audio-column.hh"
#include "audio-staff.hh"
#include "performance.hh"
#include "score.hh"
#include "file-results.hh"

Performance::Performance ()
{
  midi_l_ =0;
}

void
Performance::add_column (Audio_column* p)
{
  p->performance_l_ = this;
  audio_column_p_list_.bottom().add (p);
}

void
Performance::output (Midi_stream& midi_stream_r)
{
  int tracks_i = audio_staff_l_list_.size() + 1;
  // ugh
  int clocks_per_4_i = 384;
  midi_stream_r << Midi_header (1, tracks_i, clocks_per_4_i);
  output_header_track (midi_stream_r);
  int n = 1;
  for (PCursor<Audio_staff*> i (audio_staff_l_list_); i.ok(); i++)
    i->output (midi_stream_r, n++);
}

void
Performance::output_header_track (Midi_stream& midi_stream_r)
{
  Midi_track midi_track;

  // perhaps multiple text events?
  String str = String (_("Creator: "));
  if (no_timestamps_global_b)
    str += "GNU LilyPond\n";
  else
      str += get_version_str() + "\n";

  Midi_text creator (Midi_text::TEXT, str);
  midi_track.add (Moment (0), &creator);

  str = _("Automatically generated");
  if (no_timestamps_global_b)
    str += ".\n";
  else
    {
      str += _(", at ");
      time_t t (time (0));
      str += ctime (&t);
      str = str.left_str (str.length_i() - 1);
    }
  Midi_text generate (Midi_text::TEXT, str);
  midi_track.add (Moment (0), &generate);

  str = _f ("from musical definition: %s", origin_str_);

  Midi_text from (Midi_text::TEXT, str);
  midi_track.add (Moment (0), &from);

  Midi_text track_name (Midi_text::TRACK_NAME, "Track "
			+ String_convert::i2dec_str (0, 0, '0'));
  midi_track.add (Moment (0), &track_name);

  Midi_tempo tempo (midi_l_->get_tempo_i (Moment (1, 4)));
  midi_track.add (Moment (0), &tempo);

  midi_stream_r  << midi_track;
}

void
Performance::add_staff (Audio_staff* l)
{
  audio_staff_l_list_.bottom().add (l);
}

void
Performance::add_element (Audio_element *p)
{
  audio_elem_p_list_.bottom().add (p);
}

void
Performance::print() const
{
#ifndef NPRINT
  DOUT << "Performance { ";
  DOUT << "Items: ";
  for (PCursor<Audio_element*> i (audio_elem_p_list_.top ()); i.ok (); i++)
    i->print ();

  DOUT << "\ncolumns: ";
  for (PCursor<Audio_column*> i (audio_column_p_list_); i.ok(); i++)
    i->print();
  DOUT << "}\n";
#endif
}

void
Performance::process()
{
  print ();

  String out = midi_l_->get_default_output ();
  if (out.empty_b ())
    {
      
      out = default_outname_base_global;
      if (out == "-")
        out = "lelie";
      int def = midi_l_->get_next_default_count ();
      if (def)
	{
	  out += "-" + to_str (def);
	}

      out += ".midi";
    }
  
  Midi_stream midi_stream (out);
  *mlog << _f ("MIDI output to %s...", out) << endl;
  target_str_global_array.push (out);

  output (midi_stream);
  *mlog << endl;
}
