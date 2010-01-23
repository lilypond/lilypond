/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef AUDIO_COLUMN_HH
#define AUDIO_COLUMN_HH

#include "lily-proto.hh"
#include "moment.hh"
#include "std-vector.hh"
#include "audio-element.hh"

/**
   generic audio grouped vertically.
*/

class Audio_column : public Audio_element
{
public:
  Audio_column (Moment when);

  void add_audio_item (Audio_item *i);
  Moment when () const;

  vector<Audio_item*> audio_items_;
  Moment when_;
  int ticks () const ;
protected:
  void offset_when (Moment m);
  friend class Score_performer;

};

#endif // AUDIO_COLUMN_HH
