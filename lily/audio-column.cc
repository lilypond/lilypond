/*
  audio-column.cc -- implement Audio_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Jan Nieuwenhuizen <janneke@gnu.org>
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
  audio_item_l_list_.bottom().add (l);
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
  DOUT << "Audio_column {";
  DOUT << "at: " << at_mom_ << ". Contains:";
  for (PCursor<Audio_item*> i (audio_item_l_list_.top ()); i.ok (); i++)
    DOUT << classname (i.ptr ()) << ", ";
  DOUT << "\n}\n";
#endif 
}

