/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2021--2022 Daniel Eble <nine.fierce.ballads@gmail.com>

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

// a functor wrapping free ()
struct Freer
{
  void operator() (void *p) { free (p); }
};

// a std::unique_ptr<T> that releases its object with free () instead of delete
template <typename T>
using unique_stdlib_ptr = std::unique_ptr<T, Freer>;

#endif // MEMORY_HH
