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

#ifndef CONTEXT_HANDLE_HH
#define CONTEXT_HANDLE_HH

#include <memory>

class Context;

// This helps count users of a Context, which are usually Music_iterators and
// sometimes other Contexts.
//
// This falls a little short of the smart pointers typical of modern C++
// because it does not automatically decrement the reference count at the end
// of its life.  The owner of a handle must nullify it explicitly at an
// appropriate moment during music translation.  Forgetting to do so doesn't
// always cause obvious problems, but it can do things like keeping ossia
// staves alive too long.
//
// Meanwhile, this class still prevents other errors, such as forgetting to
// decrement the count of the previous context when switching to a new one.
class Context_handle final
{
public:
  Context_handle () = default;
  Context_handle (Context_handle const &);
  ~Context_handle ();

  void operator= (std::nullptr_t) { reset (); }
  void operator= (Context *c) { set (c); }
  void operator= (Context_handle const &h) { set (h.context_); }
  void reset ();

  explicit operator bool () const { return context_; }
  Context &operator* () const { return *context_; }
  Context *operator->() const { return context_; }
  Context *get () const { return context_; }
  int get_count () const;

private:
  void maybe_decrement ();
  void maybe_increment ();
  void set (Context *);

private:
  Context *context_ = nullptr;
};

#endif /* CONTEXT_HANDLE_HH */
