/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

#include <cmath>
#include <ctime>

using namespace std;

#include "dimensions.hh"
#include "file-name.hh"
#include "font-metric.hh"
#include "input.hh"
#include "lily-version.hh"
#include "main.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "paper-system.hh"
#include "scm-hash.hh"
#include "string-convert.hh"
#include "warn.hh"
#include "lily-imports.hh"


const char * const Paper_outputter::type_p_name_ = 0;

Paper_outputter::Paper_outputter (SCM port, const string &format)
{
  file_ = port;
  output_module_ = SCM_EOL;
  smobify_self ();

  string module_name = "scm output-" + format;
  output_module_ = scm_c_resolve_module (module_name.c_str ());

  /*
    Enable errors for undefined stencil routines if
     -dwarning-as-error is specified; else enable warnings.
  */
  Lily::backend_testing (output_module_);
}

Paper_outputter::~Paper_outputter ()
{
}


SCM
Paper_outputter::mark_smob () const
{
  scm_gc_mark (output_module_);
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
  return scm_eval (scm, output_module_);
}

SCM
Paper_outputter::module () const
{
  return output_module_;
}

SCM
Paper_outputter::output_scheme (SCM scm)
{
  SCM str = scheme_to_string (scm);
  if (scm_is_string (str))
    dump_string (str);
  return str;
}

SCM
paper_outputter_dump (void *po, SCM x)
{
  Paper_outputter *me = (Paper_outputter *) po;
  return me->output_scheme (x);
}

void
Paper_outputter::output_stencil (Stencil stil)
{
  interpret_stencil_expression (stil.expr (), paper_outputter_dump,
                                (void *) this, Offset (0, 0));
}

void
Paper_outputter::close ()
{
  if (ly_is_port (file_))
    {
      scm_close_port (file_);
      /*
        Remove the "warning" definitions for missing stencil
        expressions so that we start fresh with the next \book
        block.  --pmccarty
      */
      Lily::remove_stencil_warnings (output_module_);
    }
}
