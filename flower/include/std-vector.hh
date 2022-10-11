/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include <string>
#include <vector>

std::vector<std::string> string_split (std::string str, char c);
std::string string_join (std::vector<std::string> const &strs,
                         const std::string &infix);

#endif /* STD_VECTOR_HH */
