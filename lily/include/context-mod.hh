/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2011 Reinhold Kainhofer <reinhold@kainhofer.com>

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

/**
 * context-mod.hh
 * Implement a structure to store context modifications to be inserted
 * at some later point
 */

#ifndef CONTEXT_MOD_HH
#define CONTEXT_MOD_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "virtual-methods.hh"

/*
  Modifications for an interpretation context as given in the
  input.
*/
struct Context_mod
{
private:
  SCM mods_;
public:
  void add_context_mod (SCM);
  void add_context_mods (SCM);

  VIRTUAL_COPY_CONSTRUCTOR (Context_mod, Context_mod);

  SCM get_mods () const;

  Context_mod ();
  Context_mod (Context_mod const &);
  Context_mod (SCM mod_list);
  DECLARE_SIMPLE_SMOBS (Context_mod);
};

DECLARE_UNSMOB (Context_mod, context_mod);

#endif /* CONTEXT_MOD_HH */

