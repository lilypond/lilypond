/*
  audio-score.cc -- implement Audio_score

  source file of the GNU LilyPond music typesetter

  (c) 1997 Jan Nieuwenhuizen <jan@digicash.com>
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
#include "audio-score.hh"
#include "score.hh"

Audio_score::Audio_score ()
{
  midi_l_ =0;
}

void
Audio_score::add (Audio_column* p)
{
  p->audio_score_l_ = this;
  audio_column_p_list_.bottom().add (p);
}

void
Audio_score::output (Midi_stream& midi_stream_r)
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
Audio_score::output_header_track (Midi_stream& midi_stream_r)
{
  Midi_track midi_track;
  
  time_t t = time (0);

  // perhaps multiple text events?
  String str = String ("Creator: ") + get_version_str() + "\n";

  Midi_text creator (Midi_text::TEXT, str);
  midi_track.add (Moment (0), &creator);

  str = "Automatically generated at ";
  str += ctime (&t);
  str = str.left_str (str.length_i() - 1);
  str += "\n";
  Midi_text generate (Midi_text::TEXT, str);
  midi_track.add (Moment (0), &generate);

  str = "from musical definition: ";

  str += origin_str_;
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
Audio_score::add_staff (Audio_staff* l)
{
  audio_staff_l_list_.bottom().add (l);
}

void
Audio_score::add (Audio_element *p)
{
  audio_elem_p_list_.bottom().add (p);
}

void
Audio_score::print() const
{    
#ifndef NPRINT
  DOUT << "Audio_score { ";
  DOUT << "\ncolumns: ";
  for (PCursor<Audio_column*> i (audio_column_p_list_); i.ok(); i++)
	i->print();
  DOUT << "}\n";
#endif 
}

void
Audio_score::process()
{
  String out=midi_l_->outfile_str_;
  if (out == "")
    out = default_out_str_ + ".midi";

  Midi_stream midi_stream (out);
  *mlog << "MIDI output to " << out<< " ..." << endl;    

  output (midi_stream);
  *mlog << endl;
}
