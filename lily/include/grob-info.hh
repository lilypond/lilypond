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

#ifndef GROB_INFO_HH
#define GROB_INFO_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

/*
  Data container for broadcasts.
*/
class Grob_info
{
  Translator *origin_trans_;
  Grob *grob_;

public:
  Grob *grob () const { return grob_; }
  Translator *origin_translator () const { return origin_trans_; }

  Context *context () const;
  Stream_event *event_cause () const;
  Stream_event *ultimate_event_cause () const;
  Grob_info (Translator *, Grob *);
  Grob_info ();

  static bool less (Grob_info i, Grob_info j);
};

#endif // GROB_INFO_HH
