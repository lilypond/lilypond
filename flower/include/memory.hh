/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021--2023 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#ifndef MEMORY_HH
#define MEMORY_HH

#include <memory>
#include <stdlib.h>

// a functor wrapping an arbitrary free-like function
template <typename T, void (*f) (T *)>
struct Freer
{
  void operator() (void *p) { f (static_cast<T *> (p)); }
};

// a std::unique_ptr<T> that releases its object with f instead of delete
template <typename T, void (*f) (T *)>
using unique_ptr_with_freer = std::unique_ptr<T, Freer<T, f>>;

// same as Freer, but f takes void *
template <typename T, void (*f) (void *)>
struct VoidFreer
{
  void operator() (void *p) { f (p); }
};

template <typename T, void (*f) (void *)>
using unique_ptr_with_void_freer = std::unique_ptr<T, VoidFreer<T, f>>;

// specialize for free
template <typename T>
using unique_stdlib_ptr = unique_ptr_with_void_freer<T, free>;

#endif // MEMORY_HH
