/*
  audio-column.cc -- implement Audio_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
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
Audio_column::at_mom() const
{
  return at_mom_;
}

void
Audio_column::print() const
{
#ifndef NPRINT
  DEBUG_OUT << "Audio_column {";
  DEBUG_OUT << "at: " << at_mom_ << ". Contains:";
  for (int i =0; i < audio_item_l_arr_.size (); i++)
    DEBUG_OUT << classname (audio_item_l_arr_[i]) << ", ";
  DEBUG_OUT << "\n}\n";
#endif 
}

