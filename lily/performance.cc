/*
  performance.cc -- implement Performance

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include <time.h>
#include "warn.hh"
#include "string.hh"
#include "string-convert.hh"
#include "main.hh"
#include "midi-def.hh"
#include "midi-item.hh"
#include "midi-stream.hh"
#include "audio-column.hh"
#include "audio-item.hh"
#include "audio-staff.hh"
#include "performance.hh"
#include "score.hh"
#include "file-results.hh"
#include "file-path.hh"
#include "lily-version.hh"

#include "killing-cons.tcc"

Performance::Performance ()
{
  midi_ =0;
  audio_elem_p_list_ = 0;
}


Performance::~Performance ()
{
  delete audio_elem_p_list_;
}

void
Performance::output (Midi_stream& midi_stream)
{
  int tracks_i = audio_staffs_.size () + 1;

  // ugh
  int clocks_per_4_i = 384;

  midi_stream << Midi_header (1, tracks_i, clocks_per_4_i);
  output_header_track (midi_stream);
  progress_indication ("\n");
  progress_indication (_ ("Track ... "));
  int channel = 0;
  for (int i =0; i < audio_staffs_.size (); i++)
    {
      Audio_staff *s = audio_staffs_[i];
      if (verbose_global_b)
	progress_indication ("[" + to_string (i)) ;

      /*
	MIDI players tend to ignore instrument settings on
	channel 10, the percussion channel by default.
       */
      if (channel == 9)
	 channel++;
      if (s->channel_ < 0)
	 s->channel_ = channel;
      s->output (midi_stream, channel++);
      if (verbose_global_b)
	progress_indication ("]");
    }
}


void
Performance::output_header_track (Midi_stream& midi_stream)
{
  Midi_track midi_track;

  midi_track.channel_ = 9;

  // perhaps multiple text events?
  String id_string;
  String str = String (_ ("Creator: "));
  id_string = String_convert::pad_to (gnu_lilypond_version_string (), 30);
  str += id_string;

  /*
    This seems silly, but in fact the audio elements should
    be generated elsewhere: not midi-specific.
   */
  Audio_text creator_a (Audio_text::TEXT, str);
  Midi_text creator (&creator_a);
  midi_track.add (Moment (0), &creator);

  /* Better not translate this */
  str = "Generated automatically by: ";
  str += id_string;
  
  Audio_text generate_a (Audio_text::TEXT, str);
  Midi_text generate (&generate_a);
  midi_track.add (Moment (0), &generate);
  
  str = _ ("at ");
  time_t t (time (0));
  str += ctime (&t);
  str = str.left_string (str.length () - 1);
  str = String_convert::pad_to (str, 60);
  
  Audio_text at_a (Audio_text::TEXT, str);
  Midi_text at (&at_a);
  midi_track.add (Moment (0), &at);

  
  str = _f ("from musical definition: %s", origin_string_);

  Audio_text from_a (Audio_text::TEXT, str);
  Midi_text from (&from_a);
  midi_track.add (Moment (0), &from);

  Audio_text track_name_a (Audio_text::TRACK_NAME, "Track "
			   + String_convert::int2dec (0, 0, '0'));
  Midi_text track_name (&track_name_a);
			
  midi_track.add (Moment (0), &track_name);

  // Some sequencers read track 0 last.
  //  Audio_tempo tempo_a (midi_->get_tempo (Moment (Rational (1, 4))));
  //  Midi_tempo tempo (&tempo_a);
  //  midi_track.add (Moment (0), &tempo);

  midi_stream << midi_track;
}

void
Performance::add_element (Audio_element *p)
{
  if (Audio_staff*s=dynamic_cast<Audio_staff *> (p)) 
    {
      audio_staffs_.push (s);
    }
  else if (Audio_column *c = dynamic_cast<Audio_column*> (p))
    {
      c->performance_ = this;
    }
  audio_elem_p_list_ = new Killing_cons<Audio_element> (p, audio_elem_p_list_);
}


void
Performance::process ()
{
  String out = output_name_global;
  if (out == "-")
    out = "lelie.midi";
  int def = midi_->get_next_score_count ();
  if (def)
    {
      Path p = split_path (out);
      p.base += "-" + to_string (def);
      out = p.string ();
    }

  /* Maybe a bit crude, but we had this before */
  Path p = split_path (out);
  p.ext = "midi";
  out = p.string ();
  
  Midi_stream midi_stream (out);
  progress_indication (_f ("MIDI output to `%s'...", out));
  target_string_globals.push (out);

  output (midi_stream);
  progress_indication ("\n");
}
