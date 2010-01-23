/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "ly-smobs.icc"

void
Engraver_dispatch_list::apply (Grob_info gi)
{
  Translator *origin = gi.origin_translator ();
  for (vsize i = 0; i < dispatch_entries_.size (); i++)
    {
      Engraver_dispatch_entry const &e (dispatch_entries_[i]);
      if (e.engraver_ == origin)
	continue;

      (*e.function_) (e.engraver_, gi);
    }
}

SCM
Engraver_dispatch_list::create (SCM trans_list,
				SCM iface_list, Direction start_end)
{
  SCM retval = Engraver_dispatch_list ().smobbed_copy ();
  Engraver_dispatch_list *list = Engraver_dispatch_list::unsmob (retval);

  Engraver_dispatch_entry entry;
  bool found = false;
  for (SCM s = trans_list; scm_is_pair (s); s = scm_cdr (s))
    {
      Engraver *eng
	= dynamic_cast<Engraver *> (unsmob_translator (scm_car (s)));

      if (!eng)
	continue;

      entry.engraver_ = eng;
      for (SCM i = iface_list; scm_is_pair (i); i = scm_cdr (i))
	{
	  Engraver_void_function_engraver_grob_info ptr
	    = (start_end == START)
	    ? eng->get_acknowledger (scm_car (i))
	    : eng->get_end_acknowledger (scm_car (i));
	  
	  if (ptr)
	    {
	      entry.function_ = ptr;
	      list->dispatch_entries_.push_back (entry);
	      found = true;
	    }
	}
    }

  return found ? retval : SCM_EOL;
}

SCM
Engraver_dispatch_list::mark_smob (SCM)
{
  return SCM_BOOL_F;
}

int
Engraver_dispatch_list::print_smob (SCM /* x */,
				    SCM p,
				    scm_print_state *)
{
  scm_puts ("#<Engraver_dispatch_list>", p);
  return 1;
}

IMPLEMENT_SIMPLE_SMOBS (Engraver_dispatch_list);
IMPLEMENT_DEFAULT_EQUAL_P (Engraver_dispatch_list);
