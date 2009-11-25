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

#include "context-handle.hh"
#include "context.hh"

Context_handle::Context_handle ()
{
  outlet_ = 0;
}

Context_handle::Context_handle (Context_handle const &s)
{
  outlet_ = 0;
  if (s.outlet_)
    up (s.outlet_);
}

Context_handle::~Context_handle ()
{
  /*
    Don't do

    if (outlet_)
    down ();

    with GC, this is asynchronous.
  */
}

void
Context_handle::up (Context *t)
{
  outlet_ = t;
  t->iterator_count_++;
}

void
Context_handle::down ()
{
  outlet_->iterator_count_--;
  outlet_ = 0;
}

void
Context_handle::operator = (Context_handle const &s)
{
  set_context (s.outlet_);
}

void
Context_handle::set_context (Context *trans)
{
  if (outlet_ == trans)
    return;
  if (outlet_)
    down ();
  if (trans)
    up (trans);
}

Context *
Context_handle::get_outlet () const
{

  return outlet_;
}

int
Context_handle::get_count () const
{
  return outlet_->iterator_count_;
}
