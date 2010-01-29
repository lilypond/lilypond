/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2010 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "paper-system.hh"
#include "item.hh"

Prob *
make_paper_system (SCM immutable_init)
{
  Prob *prob = new Prob (ly_symbol2scm ("paper-system"), immutable_init);
  return prob;
}

void
paper_system_set_stencil (Prob *prob, Stencil s)
{
  SCM yext = prob->get_property ("Y-extent");

  if (is_number_pair (yext))
    {
      Box b = s.extent_box ();
      b[Y_AXIS] = ly_scm2interval (yext);

      s = Stencil (b, s.expr ());
    }

  prob->set_property ("stencil", s.smobbed_copy ());
}
