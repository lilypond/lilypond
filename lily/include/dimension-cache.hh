/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef DIMENSION_CACHE_HH
#define DIMENSION_CACHE_HH

#include "lily-proto.hh"

/*
  XY offset/refpoint/extent structure.
*/
class Dimension_cache
{
  Interval *extent_;
  Real *offset_;
  Grob *parent_;
  void init ();
  void clear ();
  
  friend class Grob;
  
  Dimension_cache (Dimension_cache const &);
  ~Dimension_cache ();
  Dimension_cache ();
};

#endif /* DIMENSION_CACHE_HH */

