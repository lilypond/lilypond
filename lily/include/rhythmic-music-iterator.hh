/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2012--2022 Mike Solomon <mike@mikesolomon.org>

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
  Iterator for rhythmic music events that are not enclosed by EventChord
*/
class Rhythmic_music_iterator final : public Simple_music_iterator
{
protected:
  OVERRIDE_CLASS_NAME (Rhythmic_music_iterator);

public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Rhythmic_music_iterator ();
  Rhythmic_music_iterator (Rhythmic_music_iterator const &);

protected:
  void process (Moment) override;
  void create_contexts () override;
};

#endif /* RHYTHMIC_MUSIC_ITERATOR_HH */
