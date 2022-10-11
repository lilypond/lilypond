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

#ifndef GROB_INFO_HH
#define GROB_INFO_HH

#include "grob.hh"

class Engraver;
class Stream_event;

/*
  Data container for broadcasts.
*/
template <class T>
class Grob_info_t
{
  Engraver *origin_engraver_;
  T *grob_;

public:
  // both the Engraver and the Grob are required
  Grob_info_t (Engraver *e, T *g)
    : origin_engraver_ (e),
      grob_ (g)
  {
  }

  T *grob () const { return grob_; }
  Engraver *origin_engraver () const { return origin_engraver_; }

  Stream_event *event_cause () const { return grob_->event_cause (); }

  Stream_event *ultimate_event_cause () const
  {
    return grob_->ultimate_event_cause ();
  }

  static bool less (const Grob_info_t &a, const Grob_info_t &b)
  {
    return Grob::less (a.grob (), b.grob ());
  }
};

using Grob_info = Grob_info_t<Grob>;

#endif // GROB_INFO_HH
