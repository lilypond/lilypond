/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011--2012 Mike Solomon <mike@apollinemike.com>

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

#include "column-description.hh"
#include "paper-column.hh"
#include "simple-spacer.hh"
#include "spaceable-grob.hh"
#include "spring.hh"

static Grob *
next_spaceable_column (vector<Grob *> const &list, vsize starting)
{
  for (vsize i = starting + 1; i < list.size (); i++)
    if (!Paper_column::is_loose (list[i]))
      return list[i];
  return 0;
}

Column_description
Column_description::get_column_description (vector<Grob *> const &cols, vsize col_index, bool line_starter)
{
  Grob *col = cols[col_index];
  if (line_starter)
    col = Item::maybe_find_prebroken_piece (dynamic_cast<Item *> (col), RIGHT);

  Column_description description;
  Grob *next_col = next_spaceable_column (cols, col_index);
  if (next_col)
    description.spring_ = Spaceable_grob::get_spring (col, next_col);

  Grob *end_col = dynamic_cast<Item *> (cols[col_index + 1])->find_prebroken_piece (LEFT);
  if (end_col)
    description.end_spring_ = Spaceable_grob::get_spring (col, end_col);

  for (SCM s = Spaceable_grob::get_minimum_distances (col);
       scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *other = unsmob_grob (scm_caar (s));
      vsize j = binary_search (cols, other, Paper_column::less_than, col_index);
      if (j != VPOS)
        {
          if (cols[j] == other)
            description.rods_.push_back (Rod_description (j, scm_to_double (scm_cdar (s))));
          else /* it must end at the LEFT prebroken_piece */
            description.end_rods_.push_back (Rod_description (j, scm_to_double (scm_cdar (s))));
        }
    }

  if (!line_starter && to_boolean (col->get_property ("keep-inside-line")))
    description.keep_inside_line_ = col->extent (col, X_AXIS);

  description.break_permission_ = col->get_property ("line-break-permission");
  return description;
}