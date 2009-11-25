/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cstdio>
#include "dot-configuration.hh"
#include "dot-formatting-problem.hh"
#include "staff-symbol-referencer.hh"


int
Dot_configuration::badness () const
{
  int t = 0;
  for (Dot_configuration::const_iterator i (begin ());
       i != end (); i++)
    {
      int p = i->first;
      int demerit = sqr (p - i->second.pos_) * 2;

      int dot_move_dir = sign (p - i->second.pos_);
      if (i->second.extremal_head_)
	{
	  if (i->second.dir_
	      && dot_move_dir != i->second.dir_)
	    demerit += 3;
	  else if (dot_move_dir != UP)
	    demerit += 2;
	}
      else if (dot_move_dir != UP)
	demerit += 1;

      t += demerit;
    }

  return t;
}

void
Dot_configuration::print () const
{
  printf ("dotconf { ");
  for (Dot_configuration::const_iterator i (begin ());
       i != end (); i++)
    printf ("%d, ", i->first);
  printf ("}\n");
}

/*
  Shift K and following (preceding) entries up (down) as necessary to
  prevent staffline collisions if D is up (down).

  If K is in CFG, then do nothing.
*/

Dot_configuration
Dot_configuration::shifted (int k, Direction d) const
{
  Dot_configuration new_cfg (*problem_);
  int offset = 0;

  if (d > 0)
    {
      for (Dot_configuration::const_iterator i (begin ());
	   i != end (); i++)
	{
	  int p = i->first;
	  if (p == k)
	    {
	      if (Staff_symbol_referencer::on_line (i->second.dot_, p))
		p += d;
	      else
		p += 2* d;

	      offset = 2*d;

	      new_cfg[p] = i->second;
	    }
	  else
	    {
	      if (new_cfg.find (p) == new_cfg.end ())
		offset = 0;
	      new_cfg[p + offset] = i->second;
	    }
	}
    }
  else
    {
      Dot_configuration::const_iterator i (end ());
      do
	{
	  i--;

	  int p = i->first;
	  if (p == k)
	    {
	      if (Staff_symbol_referencer::on_line (i->second.dot_, p))
		p += d;
	      else
		p += 2* d;

	      offset = 2*d;

	      new_cfg[p] = i->second;
	    }
	  else
	    {
	      if (new_cfg.find (p) == new_cfg.end ())
		offset = 0;

	      new_cfg[p + offset] = i->second;
	    }
	}
      while (i != begin ());
    }

  return new_cfg;
}

/*
  Remove the collision in CFG either by shifting up or down, whichever
  is best.
*/
void
Dot_configuration::remove_collision (int p)
{
  bool collide = find (p) != end ();

  if (collide)
    {
      Dot_configuration cfg_up = shifted (p, UP);
      Dot_configuration cfg_down = shifted (p, DOWN);

      int b_up = cfg_up.badness ();
      int b_down = cfg_down.badness ();

      *this = (b_up < b_down) ? cfg_up : cfg_down;
    }
}

Dot_configuration::Dot_configuration (Dot_formatting_problem const &problem)
{
  problem_ = &problem;
}

Real
Dot_configuration::x_offset () const
{
  Real off = 0.0;
  for (Dot_configuration::const_iterator i (begin ());
       i != end (); i++)
    off = max (off, problem_->head_skyline_.height ((*i).first));

  return off;
}
