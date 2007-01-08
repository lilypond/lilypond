/*
  paper-outputter-scheme.cc -- implement Paper_outputter bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "paper-outputter.hh"

#include "international.hh"
#include "stencil.hh"
#include "warn.hh"

LY_DEFINE (ly_make_paper_outputter, "ly:make-paper-outputter",
	   2, 0, 0, (SCM port, SCM format),
	   "Create an outputter that evaluates within "
	   "@code{output-}@var{format}, writing to  @var{port}.")
{
  SCM_ASSERT_TYPE (ly_is_port (port), port, SCM_ARG1, __FUNCTION__,
		   "port");
  SCM_ASSERT_TYPE (scm_is_string (format), format, SCM_ARG2, __FUNCTION__,
		   "String");

  string f = ly_scm2string (format);

  string output_name = "<unknown>";

  SCM port_name = scm_port_filename (port);
  if (scm_is_string (port_name))
    output_name = ly_scm2string (port_name);

  message (_f ("Layout output to `%s'...",
	       output_name.c_str ()));

  progress_indication ("\n");
  Paper_outputter *po = new Paper_outputter (port, f);

  po->unprotect ();
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

LY_DEFINE (ly_outputter_port, "ly:outputter-port",
	   1, 0, 0, (SCM outputter),
	   "Return output port for @var{outputter}.")
{
  Paper_outputter *po = unsmob_outputter (outputter);
  SCM_ASSERT_TYPE (po, outputter, SCM_ARG1, __FUNCTION__, "Paper_outputter");

  return po->file ();
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

LY_DEFINE (ly_outputter_output_scheme, "ly:outputter-output-scheme",
	   2, 0, 0, (SCM outputter, SCM expr),
	   "Eval @var{expr} in module of @var{outputter}.")
{
  Paper_outputter *po = unsmob_outputter (outputter);
  SCM_ASSERT_TYPE (po, outputter, SCM_ARG1, __FUNCTION__, "Paper_outputter");

  po->output_scheme (expr);

  return SCM_UNSPECIFIED;
}
