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

#include "context-handle.hh"
#include "context.hh"

Context_handle::Context_handle ()
{
  context_ = 0;
}

Context_handle::Context_handle (Context_handle const &s)
{
  context_ = 0;
  if (s.context_)
    up (s.context_);
}

Context_handle::~Context_handle ()
{
  /*
    Don't do

    if (context_)
    down ();

    with GC, this is asynchronous.
  */
}

void
Context_handle::up (Context *t)
{
  context_ = t;
  t->client_count_++;
}

void
Context_handle::down ()
{
  context_->client_count_--;
  context_ = 0;
}

void
Context_handle::operator = (Context_handle const &s)
{
  set_context (s.context_);
}

void
Context_handle::set_context (Context *trans)
{
  if (context_ == trans)
    return;
  if (context_)
    down ();
  if (trans)
    up (trans);
}

Context *
Context_handle::get_context () const
{

  return context_;
}

int
Context_handle::get_count () const
{
  return context_->client_count_;
}
