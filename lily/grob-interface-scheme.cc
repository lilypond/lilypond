/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "lily-guile.hh"
#include "std-string.hh"
#include "protected-scm.hh"

static Protected_scm all_ifaces;

void
internal_add_interface (SCM a, SCM b, SCM c)
{
  if (!all_ifaces.is_bound ())
    all_ifaces = scm_c_make_hash_table (59);

  SCM entry = ly_list (a, b, c);

  scm_hashq_set_x (all_ifaces, a, entry);
}

LY_DEFINE (ly_add_interface, "ly:add-interface", 3, 0, 0,
           (SCM iface, SCM desc, SCM props),
           R"(
Add a new grob interface.  @var{iface} is the interface name, @var{desc} is the
interface description, and @var{props} is the list of user-settable properties
for the interface.
           )")
{
  LY_ASSERT_TYPE (ly_is_symbol, iface, 1);
  LY_ASSERT_TYPE (scm_is_string, desc, 2);
  LY_ASSERT_TYPE (ly_is_list, props, 3);

  internal_add_interface (iface, desc, props);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_all_grob_interfaces, "ly:all-grob-interfaces", 0, 0, 0, (),
           R"(
Return the hash table with all grob interface descriptions.
           )")
{
  return all_ifaces;
}
