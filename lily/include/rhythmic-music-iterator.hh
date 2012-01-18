/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012 Mike Solomon <mike@apollinemike.com>

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

#ifndef RHYTHMIC_MUSIC_ITERATOR_HH
#define RHYTHMIC_MUSIC_ITERATOR_HH

#include "simple-music-iterator.hh"

/*
  Iterator for atomic music objects: events are generated at the
  beginning and at the end of the music.
*/
class Rhythmic_music_iterator : public Simple_music_iterator
{
protected:
  DECLARE_CLASSNAME (Rhythmic_music_iterator);
  //Moment last_processed_mom_;

public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Rhythmic_music_iterator ();
  virtual void process (Moment);
  //virtual bool ok ()const;
  //virtual Moment pending_moment ()const;
};

#endif /* RHYTHMIC_MUSIC_ITERATOR_HH */

