/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef PAPER_SCORE_HH
#define PAPER_SCORE_HH

#include "column-x-positions.hh"
#include "music-output.hh"

#include <vector>

/* LAYOUT output */
class Paper_score : public Music_output
{
  Output_def *layout_;
  System *system_;
  SCM paper_systems_;

  mutable std::vector<Paper_column *> cols_;
  mutable std::vector<vsize> break_indices_;
  mutable std::vector<vsize> break_ranks_;

public:
  Paper_score (Output_def *);

  OVERRIDE_CLASS_NAME (Paper_score);

  Output_def *layout () const;
  System *root_system () const;

  void typeset_system (System *);
  std::vector<Column_x_positions> calc_breaking ();

  // Ascending indices into get_columns() of breakable Paper_columns
  std::vector<vsize> const &get_break_indices () const;

  // Ascending ranks of breakable Paper_columns
  std::vector<vsize> const &get_break_ranks () const;
  std::vector<Paper_column *> const &get_columns () const;
  SCM get_paper_systems ();

protected:
  void find_break_indices () const;
  void process () override;
  void derived_mark () const override;

private:
  Paper_score (Paper_score const &) = delete;
  Paper_score &operator= (Paper_score const &) = delete;
};

#endif /* PAPER_SCORE_HH */
