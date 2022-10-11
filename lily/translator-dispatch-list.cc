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

#include "translator-dispatch-list.hh"
#include "engraver.hh"

void
Engraver_dispatch_list::apply (Grob_info gi)
{
  SCM origin = gi.origin_engraver ()->self_scm ();
  SCM grob = gi.grob ()->self_scm ();
  for (vsize i = 0; i < dispatch_entries_.size (); i++)
    {
      Method_instance const &e (dispatch_entries_[i]);
      if (scm_is_eq (e.instance (), origin))
        continue;

      e (grob, origin);
    }
}

SCM
Engraver_dispatch_list::create (SCM trans_list, SCM iface_list,
                                Direction start_end)
{
  SCM retval = Engraver_dispatch_list ().smobbed_copy ();
  Engraver_dispatch_list *list = unsmob<Engraver_dispatch_list> (retval);

  for (SCM s = trans_list; scm_is_pair (s); s = scm_cdr (s))
    {
      Engraver *eng = unsmob<Engraver> (scm_car (s));

      if (!eng)
        continue;

      for (SCM i = iface_list; scm_is_pair (i); i = scm_cdr (i))
        {
          SCM ptr = eng->get_acknowledger (scm_car (i), start_end);

          if (!SCM_UNBNDP (ptr))
            list->dispatch_entries_.push_back (Method_instance (ptr, eng));
        }
    }

  return list->dispatch_entries_.empty () ? SCM_EOL : retval;
}
