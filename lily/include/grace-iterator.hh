/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef NEWGRACE_ITERATOR_HH
#define NEWGRACE_ITERATOR_HH

#include "music-wrapper-iterator.hh"

class Grace_iterator final : public Music_wrapper_iterator
{
  bool in_grace_;

public:
  Grace_iterator ()
    : in_grace_ (false)
  {
  }
  void process (Moment) override;
  DECLARE_SCHEME_CALLBACK (constructor, ());
  OVERRIDE_CLASS_NAME (Grace_iterator);
  Moment pending_moment () const override;
};

#endif /* GRACE_ITERATOR_HH */
