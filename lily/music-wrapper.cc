/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "music-wrapper.hh"

#include "music.hh"

MAKE_SCHEME_CALLBACK (Music_wrapper, start_callback, 1);
SCM
Music_wrapper::start_callback (SCM m)
{
  Music *me = unsmob<Music> (m);
  Music *elt = unsmob<Music> (me->get_property ("element"));
  if (elt)
    return elt->start_mom ().smobbed_copy ();
  else
    return Moment ().smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Music_wrapper, length_callback, 1);
SCM
Music_wrapper::length_callback (SCM m)
{
  Music *me = unsmob<Music> (m);
  Music *elt = unsmob<Music> (me->get_property ("element"));
  if (elt)
    return elt->get_length ().smobbed_copy ();
  else
    return Moment (0).smobbed_copy ();
}
