/*
  audio-item.cc -- implement Audio items.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/
#include "debug.hh"
#include "audio-item.hh"
#include "midi-item.hh"
#include "audio-column.hh"

Audio_instrument::Audio_instrument (String instrument_str)
{
  str_ = instrument_str;
}

Audio_item::Audio_item ()
{
  audio_column_l_ = 0;
}

Audio_note::Audio_note (Pitch p, Moment m, int transposing_i)
{
  pitch_ = p;
  length_mom_ = m;
  tied_ = 0;
  transposing_i_ = transposing_i;
}

void
Audio_note::tie_to (Audio_note* t)
{
  tied_ = t;
  Audio_note* first = t;
  while (first->tied_)
    first = first->tied_;
  first->length_mom_ += length_mom_;
  length_mom_ = 0;
}

		    
Audio_key::Audio_key () // Key_def const& k)
{
  //fixme.
}

Audio_dynamic::Audio_dynamic (Real volume)
{
  volume_ = volume;
}

Audio_tempo::Audio_tempo (int per_minute_4_i)
{
  per_minute_4_i_ = per_minute_4_i;
}

Audio_time_signature::Audio_time_signature (int beats, int one_beat)
{
  beats_i_ = beats;
  one_beat_i_ = one_beat;
}

Audio_text::Audio_text (Audio_text::Type type, String text_str)
{
  text_str_ = text_str;
  type_ = type;
}

Audio_tie::Audio_tie ()
{
  note_l_drul_[RIGHT] = 0;
  note_l_drul_[LEFT] = 0;
}

void
Audio_tie::set_note (Direction d, Audio_note* note_l)
{
  assert (!note_l_drul_[d]);
  note_l_drul_[d] = note_l;
  //set_bound (d, head_l);

  //  add_dependency (head_l);
}



