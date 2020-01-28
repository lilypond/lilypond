/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef TRANSLATOR_DISPATCH_LIST_HH
#define TRANSLATOR_DISPATCH_LIST_HH

#include "callback.hh"
#include "lily-proto.hh"
#include "smobs.hh"
#include "std-vector.hh"
#include "translator.hh"

class Engraver_dispatch_list : public Simple_smob<Engraver_dispatch_list>
{
  std::vector<Method_instance> dispatch_entries_;

public:
  void apply (Grob_info);
  SCM static create (SCM trans_list, SCM iface_list, Direction);
};

#endif /* TRANSLATOR_DISPATCH_LIST_HH */
