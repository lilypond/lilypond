/*
  audio-column.cc -- implement Audio_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "audio-column.hh"
#include "audio-item.hh"
#include "performance.hh"

#include "debug.hh"

Audio_column::Audio_column (Moment at_mom)
{
  at_mom_ = at_mom;
  performance_l_ = 0;
}

void
Audio_column::add_audio_item (Audio_item* l)
{
  audio_item_l_arr_.push (l);
  l->audio_column_l_ = this; 
}

Moment
Audio_column::at_mom () const
{
  return at_mom_;
}


