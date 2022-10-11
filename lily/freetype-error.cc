/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Han-Wen Nienhuys <hanwen@lilypond.org>


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

#include "freetype.hh"

#undef __FTERRORS_H__
#define FT_ERRORDEF(e, v, s) {e, s},
#define FT_ERROR_START_LIST {
#define FT_ERROR_END_LIST                                                      \
  {                                                                            \
    0, 0                                                                       \
  }                                                                            \
  }                                                                            \
  ;

const struct Freetype_error_message
{
  FT_Error err_code;
  const char *err_msg;
} ft_errors[] =

#include FT_ERRORS_H

  ;

#include <string>

using std::string;

string
freetype_error_string (FT_Error code)
{
  for (Freetype_error_message const *p = ft_errors; p->err_msg; p++)
    {
      if (p->err_code == code)
        return p->err_msg;
    }

  return "<unknown error>";
}
