/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef GRACE_FIXUP_HH
#define GRACE_FIXUP_HH

#include "moment.hh"

/*
  This is a lookahead list for grace notes.

  {  ... X \grace Y  Z ... }

  normally, the ending of X is the start of Z. In case of a grace
  note, we take off a little at the end of X. What is stored: START
  (start point of X), LENGTH (length of X), GRACE_START (start_music
  of Y), and the next fixup element.

  This is also done for nested musics, i.e.

  voiceA = \notes { \grace b16 c'2 }
  voiceB = \notes { c'2 \voiceA }

  the iterator for voiceB will contain a fixup entry with (START = 0/1,
  LENGTH =2/1, GRACE_START =(0G-1/16) )

  Graces at the start of a sequential music iterator are handled
  by initting here_mom_ with Music::start_music (); no fixups are needed.
*/
struct Grace_fixup
{
  Moment start_;
  Moment length_;

  Rational grace_start_;

  Grace_fixup *next_;
};

#endif /* GRACE_FIXUP_HH */

