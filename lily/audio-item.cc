/*
  audio-item.cc -- implement Audio items.

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "audio-item.hh"
#include "midi-item.hh"
#include "audio-column.hh"

Audio_instrument::Audio_instrument (String instrument_string)
{
  str_ = instrument_string;
}

Audio_item::Audio_item ()
{
  audio_column_ = 0;
}

Audio_note::Audio_note (Pitch p, Moment m, int transposing_i)
{
  pitch_ = p;
  length_mom_ = m;
  tied_ = 0;
  transposing_ = transposing_i;
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

		    
Audio_key::Audio_key (int acc, bool major)
{
  accidentals_=acc;
  major_=major;
}

Audio_dynamic::Audio_dynamic (Real volume)
{
  volume_ = volume;
}

Audio_tempo::Audio_tempo (int per_minute_4_i)
{
  per_minute_4_ = per_minute_4_i;
}

Audio_time_signature::Audio_time_signature (int beats, int one_beat)
{
  beats_ = beats;
  one_beat_ = one_beat;
}

Audio_text::Audio_text (Audio_text::Type type, String text_string)
{
  text_string_ = text_string;
  type_ = type;
}

Audio_tie::Audio_tie ()
{
  note_l_drul_[RIGHT] = 0;
  note_l_drul_[LEFT] = 0;
}

void
Audio_tie::set_note (Direction d, Audio_note* note)
{
  assert (!note_l_drul_[d]);
  note_l_drul_[d] = note;
  //set_bound (d, head);

  //  add_dependency (head);
}



