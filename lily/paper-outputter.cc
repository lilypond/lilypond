/*
  paper-outputter.cc -- implement Paper_outputter

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
                 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-outputter.hh"

#include <math.h>
#include <ctime>

#include "dimensions.hh"
#include "file-name.hh"
#include "font-metric.hh"
#include "input-smob.hh"
#include "lily-version.hh"
#include "ly-module.hh"
#include "main.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "paper-system.hh"
#include "scm-hash.hh"
#include "string-convert.hh"
#include "warn.hh"

#include "ly-smobs.icc"

Paper_outputter::Paper_outputter (String file_name, String format)
{
  file_ = SCM_EOL;
  output_module_ = SCM_EOL;
  smobify_self ();
  
  file_name_ = file_name;
  String module_name = "scm output-" + format;
  output_module_ = scm_c_resolve_module (module_name.to_str0 ());
}

Paper_outputter::~Paper_outputter ()
{
}


IMPLEMENT_SMOBS (Paper_outputter);
IMPLEMENT_DEFAULT_EQUAL_P (Paper_outputter);

SCM
Paper_outputter::mark_smob (SCM x)
{
  Paper_outputter *p = (Paper_outputter*) SCM_CELL_WORD_1(x);
  scm_gc_mark (p->output_module_);
  return p->file_;
}

int
Paper_outputter::print_smob (SCM x, SCM p, scm_print_state*)
{
  (void) x;
  scm_puts ("#<Paper_outputter>", p);
  return 1;
}

SCM
Paper_outputter::file ()
{
  if (file_ == SCM_EOL)
    if (file_name_ == "-")
      file_ = scm_current_output_port();
    else
      file_ = scm_open_file (scm_makfrom0str (file_name_.to_str0 ()),
			     scm_makfrom0str ("w"));
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
  Paper_outputter *me = (Paper_outputter*) po;
  me->output_scheme (x);
}

void
Paper_outputter::output_stencil (Stencil stil)
{
  interpret_stencil_expression (stil.expr (), paper_outputter_dump,
                                (void*) this, Offset (0,0));
}

LY_DEFINE (ly_make_paper_outputter, "ly:make-paper-outputter",
	   2, 0, 0, (SCM outname, SCM format),
	   "Create an outputter that evaluates within "
	   "@code{output-}@var{format}, writing to file @var{outname}.")
{
  SCM_ASSERT_TYPE(scm_is_string (outname), outname, SCM_ARG1, __FUNCTION__,
		  "String");
  SCM_ASSERT_TYPE(scm_is_string (format), format, SCM_ARG2, __FUNCTION__,
		  "String");
  
  String outname_str = ly_scm2string (outname);
  String f = ly_scm2string (format);

  progress_indication (_f ("Layout output to `%s'...",
			   outname_str == "-"
			   ? String ("<stdout>")
			   : outname_str));
  progress_indication ("\n");
  Paper_outputter *po = new Paper_outputter (outname_str, f);

  scm_gc_unprotect_object (po->self_scm ());
  return po->self_scm ();
}

/* FIXME: why is output_* wrapper called dump?  */
LY_DEFINE (ly_outputter_dump_stencil, "ly:outputter-dump-stencil",
	   2, 0, 0, (SCM outputter, SCM stencil),
	   "Dump stencil @var{expr} onto @var{outputter}.")
{
  Paper_outputter *po = unsmob_outputter (outputter);
  Stencil *st = unsmob_stencil (stencil);
  SCM_ASSERT_TYPE (po, outputter, SCM_ARG1, __FUNCTION__, "Paper_outputter");
  SCM_ASSERT_TYPE (st, stencil, SCM_ARG1, __FUNCTION__, "Paper_outputter");
  po->output_stencil (*st);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_outputter_dump_string, "ly:outputter-dump-string",
	   2, 0, 0, (SCM outputter, SCM str),
	   "Dump @var{str} onto @var{outputter}.")
{
  Paper_outputter *po = unsmob_outputter (outputter);
  SCM_ASSERT_TYPE (po, outputter, SCM_ARG1, __FUNCTION__, "Paper_outputter");
  SCM_ASSERT_TYPE (scm_is_string (str), str, SCM_ARG1, __FUNCTION__, "Paper_outputter");
  
  return po->dump_string (str);
}


LY_DEFINE (ly_outputter_close, "ly:outputter-close",
	   1, 0, 0, (SCM outputter),
	   "Close port of @var{outputter}.")
{
  Paper_outputter *po = unsmob_outputter (outputter);
  SCM_ASSERT_TYPE (po, outputter, SCM_ARG1, __FUNCTION__, "Paper_outputter");

  po->close ();
  return SCM_UNSPECIFIED;
}

void
Paper_outputter::close ()
{
  if (scm_port_p (file_) == SCM_BOOL_T)
    scm_close_port (file_);
}
