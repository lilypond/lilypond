/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
#include "lily-version.hh"
#include "main.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "paper-system.hh"
#include "scm-hash.hh"
#include "stencil-interpret.hh"
#include "string-convert.hh"
#include "warn.hh"

#include <cmath>
#include <ctime>

using std::string;

Paper_outputter::Paper_outputter (SCM port, SCM callback)
{
  file_ = port;
  callback_ = SCM_EOL;
  smobify_self ();

  callback_ = callback;
}

Paper_outputter::~Paper_outputter ()
{
}

SCM
Paper_outputter::mark_smob () const
{
  scm_gc_mark (callback_);
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
Paper_outputter::scheme_to_string (SCM scm)
{
  return scm_call_1 (callback_, scm);
}

SCM
Paper_outputter::output_scheme (SCM scm)
{
  SCM str = scheme_to_string (scm);
  if (scm_is_string (str))
    dump_string (str);
  return str;
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
