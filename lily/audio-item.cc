/*
  audio-item.cc -- implement Audio items.

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
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

Audio_note::Audio_note (Musical_pitch p, Moment m, int transposing_i)
{
  pitch_ = p;
  length_mom_ = m;
  transposing_i_ = transposing_i;
}

Audio_key::Audio_key (Key_def const& k)
{
  key_ = k;
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
  //set_bounds (d, head_l);

  //  add_dependency (head_l);
}

void
Audio_item::do_print () const
{
#ifndef NPRINT
  if (audio_column_l_)
    {
      DOUT << "at: "<< audio_column_l_->at_mom ();
    }
#endif
}

