/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2024 Daniel Eble <nine.fierce.ballads@gmail.com>

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

#include "deprecated-property.hh"

#include "international.hh"
#include "lily-guile.hh"
#include "lily-imports.hh"
#include "warn.hh"

SCM
Deprecated_property::getter_desc (SCM old_sym, SCM obj_prop)
{
  SCM desc = ly_call (scm_procedure (obj_prop), old_sym);
  if (!scm_is_false (desc))
    {
      SCM compat_warned_sym = ly_symbol2scm ("warned-for-deprecated-access");
      SCM warned = scm_object_property (old_sym, compat_warned_sym);
      if (!scm_is_true (warned))
        {
          scm_set_object_property_x (old_sym, compat_warned_sym, SCM_BOOL_T);
          SCM new_sym = scm_car (desc);
          SCM special_warning = scm_caddr (desc);
          if (is_scm<std::string> (special_warning))
            {
              warning (from_scm<std::string> (special_warning));
            }
          else
            {
              warning (_f ("the property '%s' is deprecated; use '%s'",
                           ly_symbol2string (old_sym).c_str (),
                           ly_symbol2string (new_sym).c_str ()));
            }
        }
    }
  return desc;
}

SCM
Deprecated_property::setter_desc (SCM old_sym, SCM obj_prop)
{
  SCM desc = ly_call (scm_procedure (obj_prop), old_sym);
  if (!scm_is_false (desc))
    {
      SCM compat_warned_sym = ly_symbol2scm ("warned-for-deprecated-access");
      SCM warned = scm_object_property (old_sym, compat_warned_sym);
      if (!scm_is_true (warned))
        {
          scm_set_object_property_x (old_sym, compat_warned_sym, SCM_BOOL_T);
          SCM new_sym = scm_caddr (desc);
          SCM special_warning = scm_cadddr (desc);
          if (is_scm<std::string> (special_warning))
            {
              warning (from_scm<std::string> (special_warning));
            }
          else
            {
              warning (_f ("the property '%s' is deprecated; use '%s'",
                           ly_symbol2string (old_sym).c_str (),
                           ly_symbol2string (new_sym).c_str ()));
            }
        }
    }
  return desc;
}
