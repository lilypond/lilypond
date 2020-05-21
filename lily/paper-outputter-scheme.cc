/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "international.hh"
#include "stencil.hh"
#include "warn.hh"

using std::string;

LY_DEFINE (ly_make_paper_outputter, "ly:make-paper-outputter",
           2, 0, 0, (SCM port, SCM format),
           "Create an outputter that evaluates within"
           " @code{output-}@var{format}, writing to @var{port}.")
{
  LY_ASSERT_TYPE (ly_is_port, port, 1);
  LY_ASSERT_TYPE (ly_is_symbol, format, 2);

  string f = ly_symbol2string (format);
  string output_name = "<unknown>";

  SCM port_name = scm_port_filename (port);
  if (scm_is_string (port_name))
    output_name = ly_scm2string (port_name);

  message (_f ("Layout output to `%s'...",
               output_name.c_str ()));

  progress_indication ("\n");
  Paper_outputter *po = new Paper_outputter (port, f);

#if GUILEV2
  if (f == "ps")
    scm_set_port_encoding_x (port, ly_string2scm ("ISO-8859-1"));
#endif

  po->unprotect ();
  return po->self_scm ();
}

LY_DEFINE (ly_outputter_dump_stencil, "ly:outputter-dump-stencil",
           2, 0, 0, (SCM outputter, SCM stencil),
           "Dump stencil @var{expr} onto @var{outputter}.")
{

  LY_ASSERT_SMOB (Paper_outputter, outputter, 1);
  LY_ASSERT_SMOB (Stencil, stencil, 2);

  Paper_outputter *po = unsmob<Paper_outputter> (outputter);
  Stencil *st = unsmob<Stencil> (stencil);

  po->output_stencil (*st);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_outputter_dump_string, "ly:outputter-dump-string",
           2, 0, 0, (SCM outputter, SCM str),
           "Dump @var{str} onto @var{outputter}.")
{
  LY_ASSERT_SMOB (Paper_outputter, outputter, 1);
  LY_ASSERT_TYPE (scm_is_string, str, 2);

  Paper_outputter *po = unsmob<Paper_outputter> (outputter);

  return po->dump_string (str);
}

LY_DEFINE (ly_outputter_port, "ly:outputter-port",
           1, 0, 0, (SCM outputter),
           "Return output port for @var{outputter}.")
{
  LY_ASSERT_SMOB (Paper_outputter, outputter, 1);
  Paper_outputter *po = unsmob<Paper_outputter> (outputter);

  return po->file ();
}

LY_DEFINE (ly_outputter_close, "ly:outputter-close",
           1, 0, 0, (SCM outputter),
           "Close port of @var{outputter}.")
{
  LY_ASSERT_SMOB (Paper_outputter, outputter, 1);
  Paper_outputter *po = unsmob<Paper_outputter> (outputter);

  po->close ();
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_outputter_output_scheme, "ly:outputter-output-scheme",
           2, 0, 0, (SCM outputter, SCM expr),
           "Eval @var{expr} in module of @var{outputter}.")
{
  LY_ASSERT_SMOB (Paper_outputter, outputter, 1);
  Paper_outputter *po = unsmob<Paper_outputter> (outputter);

  po->output_scheme (expr);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_outputter_module, "ly:outputter-module",
           1, 0, 0, (SCM outputter),
           "Return output module of @var{outputter}.")
{
  LY_ASSERT_SMOB (Paper_outputter, outputter, 1);

  Paper_outputter *po = unsmob<Paper_outputter> (outputter);
  return po->module ();
}
