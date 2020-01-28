/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef CHORD_TREMOLO_ITERATOR_HH
#define CHORD_TREMOLO_ITERATOR_HH

#include "sequential-iterator.hh"

class Chord_tremolo_iterator : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  /* construction */
  OVERRIDE_CLASS_NAME (Chord_tremolo_iterator);
  Chord_tremolo_iterator ();

protected:
  SCM get_music_list () const override;

private:
};

#endif /* CHORD_TREMOLO_ITERATOR_HH */
