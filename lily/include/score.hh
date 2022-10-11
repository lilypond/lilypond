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

#ifndef SCORE_HH
#define SCORE_HH

#include "lily-proto.hh"

#include "input.hh"
#include "smobs.hh"
#include "virtual-methods.hh"

#include <vector>

class Score : public Smob<Score>
{
public:
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Score ();

private:
  SCM music_;
  SCM input_location_;
  SCM header_;

public:
  Input *origin () const;

  std::vector<Output_def *> defs_;
  bool error_found_;

  Score ();
  Score (Score const &);

  VIRTUAL_CLASS_NAME (Score);
  virtual Score *clone () const { return new Score (*this); }

  SCM get_music () const;
  void add_output_def (Output_def *def);
  void set_music (SCM music);
  SCM book_rendering (Output_def *, Output_def *);
  SCM get_header () const;
  void set_header (SCM module);
};

SCM ly_run_translator (SCM, SCM);

#endif /* SCORE_HH */
