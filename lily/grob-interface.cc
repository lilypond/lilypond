/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "grob-interface.hh"

#include "grob.hh"
#include "international.hh"
#include "protected-scm.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "misc.hh"

using std::string;

SCM
add_interface (char const *cxx_name, char const *descr, char const *vars)
{
  string suffix ("-interface");
  string lispy_name = camel_case_to_lisp_identifier (cxx_name);
  vsize end = (lispy_name.size () >= suffix.size ())
                ? (lispy_name.size () - suffix.size ())
                : 0;
  if (lispy_name.substr (end) != suffix)
    lispy_name += suffix;

  SCM s = ly_symbol2scm (lispy_name);
  SCM d = scm_from_utf8_string (descr);
  SCM l = parse_symbol_list (vars);

  internal_add_interface (s, d, l);

  return s;
}

void
check_interfaces_for_property (Grob const *me, SCM sym)
{
  if (scm_is_eq (sym, ly_symbol2scm ("meta")))
    {
      /*
        otherwise we get in a nasty recursion loop.
      */
      return;
    }

  SCM ifs = me->interfaces ();

  SCM all_ifaces = ly_all_grob_interfaces ();
  bool found = false;
  for (; !found && scm_is_pair (ifs); ifs = scm_cdr (ifs))
    {
      SCM iface = scm_hashq_ref (all_ifaces, scm_car (ifs), SCM_BOOL_F);
      if (scm_is_false (iface))
        {
          programming_error (_f ("Unknown interface `%s'",
                                 ly_symbol2string (scm_car (ifs)).c_str ()));
          continue;
        }

      found = found || scm_is_true (scm_c_memq (sym, scm_caddr (iface)));
    }

  if (!found)
    {
      programming_error (_f ("Grob `%s' has no interface for property `%s'",
                             me->name ().c_str (),
                             ly_symbol2string (sym).c_str ()));
    }
}
