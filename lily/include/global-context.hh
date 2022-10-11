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

#ifndef GLOBAL_CONTEXT_HH
#define GLOBAL_CONTEXT_HH

#include "context.hh"
#include "pqueue.hh"

struct Preinit_Global_context
{
  Output_def *output_def_ = nullptr;
};

class Global_context : Preinit_Global_context, public Context
{
  PQueue<Moment> extra_mom_pq_;
  void derived_mark () const override;

  OVERRIDE_CLASS_NAME (Global_context);

  friend class Output_def;

public:
  Global_context (Output_def *, Context_def *);
  vsize get_moments_left () const;
  Moment sneaky_insert_extra_moment (Moment);
  void add_moment_to_process (Moment);
  bool iterate (Music *, bool force_found_music);

  bool is_accessible_to_user () const override { return false; }

  void apply_finalizations ();
  void add_finalization (SCM);

  void prepare (SCM);
  virtual SCM get_output ();
  Output_def *get_output_def () const override { return output_def_; }
  Moment now_mom () const override { return now_mom_; }

  Moment previous_moment () const;

private:
  Context *get_score_context () const;

private:
  Moment prev_mom_;
  Moment now_mom_;
};

// If the given context is null, return null.  Otherwise, starting from the
// given context, find the top context, expecting it to be a Global_context.
// If it is not a Global_context, abort the program.
Global_context *find_global_context (Context *where);

SCM ly_format_output (SCM);

#endif // GLOBAL_CONTEXT_HH
