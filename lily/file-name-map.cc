/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <map>
using namespace std;

#include "file-name-map.hh"
#include "lily-guile.hh"

map<string, string> file_name_map_global;

string
map_file_name (string s)
{
  if (file_name_map_global.find (s) != file_name_map_global.end ())
    s = file_name_map_global[s];
  return s;
}

LY_DEFINE (ly_add_file_name_alist, "ly:add-file-name-alist",
	   1, 0, 0, (SCM alist),
	   "Add mappings for error messages from @var{alist}.")
{
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM key = scm_caar (s);
      SCM val = scm_cdar (s);

      file_name_map_global[ly_scm2string (key)] = ly_scm2string (val);
    }
  return SCM_UNSPECIFIED;
}

