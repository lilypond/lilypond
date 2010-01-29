/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "simple-music-iterator.hh"

#include "music.hh"
#include "input.hh"

Simple_music_iterator::Simple_music_iterator ()
  : Music_iterator ()
{
  last_processed_mom_ = -1;
}

bool
Simple_music_iterator::ok ()const
{
  return last_processed_mom_ < music_get_length ();
}

/*
  TODO: remove last_processed_mom_, and the complete shit.  We should
  only process a simple-music once, and that is at its start.

  Engravers can detect and event the end-moments to be processed as
  well.
*/
Moment
Simple_music_iterator::pending_moment ()const
{
  if (last_processed_mom_ < Moment (0))
    return Moment (0);
  else
    return music_get_length ();
}

void
Simple_music_iterator::process (Moment m)
{
  /*
    don't do report_event (), since it would make the function useless for
    base classes
  */

  last_processed_mom_ = m;
}

IMPLEMENT_CTOR_CALLBACK (Simple_music_iterator);
