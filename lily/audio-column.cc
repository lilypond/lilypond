/*
  audio-column.cc -- implement Audio_column

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "audio-column.hh"

#include "audio-item.hh"
#include "performance.hh"

Audio_column::Audio_column (Moment when)
{
  when_ = when;
}

void
Audio_column::add_audio_item (Audio_item *l)
{
  audio_items_.push_back (l);
  l->audio_column_ = this;
}

Moment
Audio_column::when () const
{
  return when_;
}

int
Audio_column::ticks () const
{
  return int (moment_to_ticks (when_));
}

void
Audio_column::offset_when (Moment m)
{
  when_ += m;
}


