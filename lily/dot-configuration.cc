/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  for (const auto &ent : *this)
    {
      int p = ent.first;
      int demerit = sqr (p - ent.second.pos_) * 2;

      const Direction dot_move_dir (p - ent.second.pos_);
      if (ent.second.dir_ && dot_move_dir != ent.second.dir_)
        demerit += 2;
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
  for (const auto &ent : *this)
    printf ("%d, ", ent.first);
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

  auto process_entry = [d, k, &new_cfg, &offset] (const value_type &ent) {
    int p = ent.first;
    if (p == k)
      {
        if (Staff_symbol_referencer::on_line (ent.second.dot_, p))
          p += d;
        else
          p += 2 * d;

        offset = 2 * d;

        new_cfg[p] = ent.second;
      }
    else
      {
        if (new_cfg.find (p) == new_cfg.end ())
          offset = 0;
        new_cfg[p + offset] = ent.second;
      }
  };

  if (d > CENTER)
    {
      for (const auto &ent : *this)
        process_entry (ent);
    }
  else
    {
      for (auto i = rbegin (); i != rend (); ++i)
        process_entry (*i);
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

      swap ((b_up < b_down) ? cfg_up : cfg_down);
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
  for (const auto &ent : *this)
    off = std::max (off, problem_->head_skyline ().height (ent.first));

  return off;
}
