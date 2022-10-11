/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys

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
#ifndef COMPARE_HH
#define COMPARE_HH

#define ONE_OPERATOR(type, function, op)                                       \
  inline bool operator op (type t1, type t2)                                   \
  {                                                                            \
    return function (t1, t2) op 0;                                             \
  }

/**  handy notations for a signed comparison.
     make the operators{<,<=,==,>=,>} and the MAX and MIN of two.
     Please fill a & in the type argument if necessary.
*/
#define TEMPLATE_INSTANTIATE_COMPARE(type, function, prefix)                   \
  prefix ONE_OPERATOR (type, function, >) prefix ONE_OPERATOR (type, function, \
                                                               >=) prefix      \
  ONE_OPERATOR (type, function, ==) prefix                                     \
  ONE_OPERATOR (type, function, !=) prefix                                     \
  ONE_OPERATOR (type, function, <) prefix                                      \
  ONE_OPERATOR (type, function, <=) /* stupid fix to allow ; */                \
    prefix bool                                                                \
    operator<(type t1, type t2)

#define INSTANTIATE_COMPARE(type, func)                                        \
  TEMPLATE_INSTANTIATE_COMPARE (type, func, )

#endif
