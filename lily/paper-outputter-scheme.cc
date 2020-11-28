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

LY_DEFINE (ly_make_paper_outputter, "ly:make-paper-outputter", 2, 1, 0,
           (SCM port, SCM alist, SCM default_callback),
           "Create an outputter dumping to @code{port}. @code{alist} should map "
           "symbols to procedures. See @code{output-ps.scm} for an example. "
           "If @code{default_callback} is given, it is called for unsupported "
           "expressions")
{
  LY_ASSERT_TYPE (ly_is_port, port, 1);
  LY_ASSERT_TYPE (ly_cheap_is_list, alist, 2);
  if (default_callback != SCM_UNDEFINED && !ly_is_procedure (default_callback))
    {
      scm_wrong_type_arg_msg (mangle_cxx_identifier (__FUNCTION__).c_str (), 3,
                              default_callback, "procedure");
    }

  Paper_outputter *po = new Paper_outputter (port, alist, default_callback);

  po->unprotect ();
  return po->self_scm ();
}

LY_DEFINE (ly_outputter_dump_stencil, "ly:outputter-dump-stencil",
           2, 0, 0, (SCM outputter, SCM stencil),
           "Dump stencil @var{expr} onto @var{outputter}.")
{

  auto *const po = LY_ASSERT_SMOB (Paper_outputter, outputter, 1);
  auto *const st = LY_ASSERT_SMOB (const Stencil, stencil, 2);

  po->output_stencil (*st);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_outputter_dump_string, "ly:outputter-dump-string",
           2, 0, 0, (SCM outputter, SCM str),
           "Dump @var{str} onto @var{outputter}.")
{
  auto *const po = LY_ASSERT_SMOB (Paper_outputter, outputter, 1);
  LY_ASSERT_TYPE (scm_is_string, str, 2);

  return po->dump_string (str);
}

LY_DEFINE (ly_outputter_port, "ly:outputter-port",
           1, 0, 0, (SCM outputter),
           "Return output port for @var{outputter}.")
{
  auto *const po = LY_ASSERT_SMOB (Paper_outputter, outputter, 1);

  return po->file ();
}

LY_DEFINE (ly_outputter_close, "ly:outputter-close",
           1, 0, 0, (SCM outputter),
           "Close port of @var{outputter}.")
{
  auto *const po = LY_ASSERT_SMOB (Paper_outputter, outputter, 1);

  po->close ();
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_outputter_output_scheme, "ly:outputter-output-scheme", 2, 0, 0,
           (SCM outputter, SCM expr),
           "Output @var{expr} to the paper outputter.")
{
  auto *const po = LY_ASSERT_SMOB (Paper_outputter, outputter, 1);

  po->output_scheme (expr);

  return SCM_UNSPECIFIED;
}
