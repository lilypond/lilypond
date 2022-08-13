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

#include <cassert>

Context_handle::Context_handle (Context_handle const &s)
  : context_ (s.context_)
{
  maybe_increment ();
}

Context_handle::~Context_handle ()
{
  // Garbage collection may have delayed destruction of the owner of this
  // handle, making RAII unreliable.
  //
  // The owner of a handle must nullify it explicitly at an appropriate moment
  // during music translation.  Forgetting to do so doesn't always cause
  // obvious problems, but it can do things like keeping ossia staves alive too
  // long, so we want to detect it as early as possible.
  assert (!context_);
}

void
Context_handle::maybe_increment ()
{
  if (context_)
    ++context_->client_count_;
}

void
Context_handle::maybe_decrement ()
{
  if (context_)
    --context_->client_count_;
}

void
Context_handle::reset ()
{
  maybe_decrement ();
  context_ = nullptr;
}

void
Context_handle::set (Context *c)
{
  if (context_ != c)
    {
      maybe_decrement ();
      context_ = c;
      maybe_increment ();
    }
}

int
Context_handle::get_count () const
{
  return context_->client_count_;
}
