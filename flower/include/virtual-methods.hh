/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef VIRTUAL_METHODS_HH
#define VIRTUAL_METHODS_HH

#include <type_traits>

#define VIRTUAL_CLASS_NAME(name)                                               \
  virtual const char *class_name () const                                      \
  {                                                                            \
    /* It is annoying that we must repeat the class name for */                \
    /* the preprocessor, but we can check that it is correct. */               \
    typedef std::decay<decltype (*this)>::type self_type;                      \
    static_assert (std::is_same<name, self_type>::value, "");                  \
                                                                               \
    return #name;                                                              \
  }

#define OVERRIDE_CLASS_NAME(name)                                              \
  const char *class_name () const override                                     \
  {                                                                            \
    /* It is annoying that we must repeat the class name for */                \
    /* the preprocessor, but we can check that it is correct. */               \
    typedef std::decay<decltype (*this)>::type self_type;                      \
    static_assert (std::is_same<name, self_type>::value, "");                  \
                                                                               \
    return #name;                                                              \
  }

#endif /* VIRTUAL_METHODS_HH */
