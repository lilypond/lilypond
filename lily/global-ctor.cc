/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "global-ctor.hh"

#include "std-vector.hh"

static vector<Global_ctor> *ctor_global_statics_;

void
add_constructor (Global_ctor c)
{
  if (!ctor_global_statics_)
    ctor_global_statics_ = new vector<Global_ctor>;
  ctor_global_statics_->push_back (c);
}

void
call_constructors ()
{
  for (vsize i = 0; i < ctor_global_statics_->size (); i++)
    (ctor_global_statics_->at (i)) ();
}
