/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef EVENT_CHORD_ITERATOR_HH
#define EVENT_CHORD_ITERATOR_HH

#include "simple-music-iterator.hh"

/**
   Walk through an Event_chord
*/
class Event_chord_iterator final : public Simple_music_iterator
{
  /**
     Find a bottom notation context to deliver events to.
  */
  OVERRIDE_CLASS_NAME (Event_chord_iterator);

public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Event_chord_iterator ();
  Event_chord_iterator (Event_chord_iterator const &);

protected:
  void process (Moment) override;
  void create_contexts () override;
};

#endif // EVENT_CHORD_ITERATOR_HH
