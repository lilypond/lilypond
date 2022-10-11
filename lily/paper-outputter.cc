/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "paper-outputter.hh"

#include "dimensions.hh"
#include "file-name.hh"
#include "font-metric.hh"
#include "input.hh"
#include "international.hh"
#include "lily-imports.hh"
#include "output-def.hh"
#include "paper-system.hh"
#include "scm-hash.hh"
#include "stencil-interpret.hh"
#include "string-convert.hh"
#include "warn.hh"

#include <cmath>
#include <ctime>

using std::string;

Paper_outputter::Paper_outputter (SCM port, SCM alist, SCM default_callback)
{
  file_ = port;
  callback_tab_ = SCM_EOL;
  default_callback_ = SCM_EOL;
  smobify_self ();

  callback_tab_ = alist_to_hashq (alist);
  default_callback_ = default_callback;
  if (!ly_is_procedure (default_callback_))
    default_callback_ = SCM_BOOL_F;
}

Paper_outputter::~Paper_outputter ()
{
}

SCM
Paper_outputter::mark_smob () const
{
  scm_gc_mark (callback_tab_);
  scm_gc_mark (default_callback_);
  return file_;
}

SCM
Paper_outputter::file () const
{
  return file_;
}

SCM
Paper_outputter::dump_string (SCM scm)
{
  return scm_display (scm, file ());
}

SCM
Paper_outputter::output_scheme (SCM expr)
{
  SCM head = scm_car (expr);
  SCM callback = scm_hashq_ref (callback_tab_, head, SCM_BOOL_F);
  SCM result = SCM_BOOL_F;
  if (scm_is_true (callback))
    {
      result = scm_apply_0 (callback, scm_cdr (expr));
      if (scm_is_string (result))
        dump_string (result);
    }
  else if (scm_is_true (default_callback_))
    {
      result = ly_call (default_callback_, expr);
      if (scm_is_string (result))
        dump_string (result);
    }

  return result;
}

struct Scm_to_file : Stencil_sink
{
  Paper_outputter *po_;

  virtual SCM output (SCM scm) override { return po_->output_scheme (scm); }
};

void
Paper_outputter::output_stencil (Stencil stil)
{
  Scm_to_file stf;
  stf.po_ = this;

  interpret_stencil_expression (stil.expr (), &stf, Offset (0, 0));
}

void
Paper_outputter::close ()
{
  if (ly_is_port (file_))
    {
      scm_close_port (file_);
    }

  debug_output (
    _f ("Paper_outputter elapsed time: %.2f seconds", timer_.read ()));
}
