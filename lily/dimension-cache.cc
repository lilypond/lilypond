/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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


#include "dimension-cache.hh"

#include "warn.hh"
#include "grob.hh"

Dimension_cache::Dimension_cache (Dimension_cache const &d)
{
  init ();
  offset_ = d.offset_ ? new Real (*d.offset_) : 0;
  parent_ = d.parent_;
  extent_ = d.extent_ ? new Interval (*d.extent_) : 0;
}

Dimension_cache::Dimension_cache ()
{
  init ();
}

void
Dimension_cache::init ()
{
  offset_ = 0;
  extent_ = 0;
  parent_ = 0;
}

Dimension_cache::~Dimension_cache ()
{
  clear ();
}

void
Dimension_cache::clear ()
{
  delete extent_;
  delete offset_;
  extent_ = 0;
  offset_ = 0;
}
