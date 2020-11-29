/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef STD_VECTOR_HH
#define STD_VECTOR_HH

#if 0

/*
  leads to dubious crashes - libstdc++  bug?
*/
#ifdef DEBUG
#define _GLIBCXX_DEBUG 1
#endif
#endif

#include <algorithm>   /* find, reverse, sort */
#include <functional>  /* unary_function */
#include <cassert>
#include <string>

#include "compare.hh"

#ifndef VSIZE
#define VSIZE
typedef size_t vsize;
#define VPOS ((vsize) -1)
#endif

#include <vector>

template<typename T> struct del : public std::unary_function<T, void>
{
  void operator () (T x)
  {
    delete x;
    x = 0;
  }
};

template<typename T>
void
junk_pointers (std::vector<T> &v)
{
  // Hmm.
  for_each (v.begin (), v.end (), del<T> ());
  v.clear ();
}

std::vector<std::string> string_split (std::string str, char c);
std::string string_join (std::vector<std::string> const &strs, const std::string &infix);

#endif /* STD_VECTOR_HH */
