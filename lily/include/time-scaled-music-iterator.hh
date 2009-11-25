/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>,
                 Erik Sandberg <mandolaerik@gmail.com>

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

#ifndef TIME_SCALED_MUSIC_ITERATOR_HH
#define TIME_SCALED_MUSIC_ITERATOR_HH

#include "sequential-iterator.hh"

class Time_scaled_music_iterator : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  /* construction */
  DECLARE_CLASSNAME(Time_scaled_music_iterator);
  Time_scaled_music_iterator ();
protected:
  virtual SCM get_music_list () const;
private:
};

#endif /* TIME_SCALED_MUSIC_ITERATOR_HH */
