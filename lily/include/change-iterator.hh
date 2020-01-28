/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef CHANGE_ITERATOR_HH
#define CHANGE_ITERATOR_HH

#include "simple-music-iterator.hh"

class Change_iterator : public Simple_music_iterator
{
public:
  /* constructor is public */
  void process (Moment) override;
  DECLARE_SCHEME_CALLBACK (constructor, ());
  OVERRIDE_CLASS_NAME (Change_iterator);

  // returns an error message (empty on success)
  static std::string change_to (Music_iterator &it, SCM to_type,
                                const std::string &to_id);

private:
  void error (const std::string &);
};

#endif
