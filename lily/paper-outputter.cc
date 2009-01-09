/*
  paper-outputter.cc -- implement Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
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

#include "ly-smobs.icc"

Paper_outputter::Paper_outputter (SCM port, string format)
{
  file_ = port;
  output_module_ = SCM_EOL;
  smobify_self ();

  string module_name = "scm output-" + format;
  output_module_ = scm_c_resolve_module (module_name.c_str ());
}

Paper_outputter::~Paper_outputter ()
{
}

IMPLEMENT_SMOBS (Paper_outputter);
IMPLEMENT_DEFAULT_EQUAL_P (Paper_outputter);

SCM
Paper_outputter::mark_smob (SCM x)
{
  Paper_outputter *p = (Paper_outputter *) SCM_CELL_WORD_1 (x);
  scm_gc_mark (p->output_module_);
  return p->file_;
}

int
Paper_outputter::print_smob (SCM /* x */,
			     SCM p,
			     scm_print_state *)
{
  scm_puts ("#<Paper_outputter>", p);
  return 1;
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

void
Paper_outputter::output_scheme (SCM scm)
{
  dump_string (scheme_to_string (scm));
}

void
paper_outputter_dump (void *po, SCM x)
{
  Paper_outputter *me = (Paper_outputter *) po;
  me->output_scheme (x);
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
  if (scm_port_p (file_) == SCM_BOOL_T)
    scm_close_port (file_);
}
