/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2014 Han-Wen Nienhuys <hanwen@xs4all.nl>


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
#include "simple-closure.hh"
#include "unpure-pure-container.hh"

#include "grob.hh"

SCM
evaluate_args (SCM delayed_argument, SCM args, bool pure, int start, int end)
{
  SCM new_args = SCM_EOL;
  SCM *tail = &new_args;
  for (SCM s = args; scm_is_pair (s); s = scm_cdr (s))
    {
      *tail = scm_cons (evaluate_with_simple_closure (delayed_argument, scm_car (s),
                                                      pure, start, end),
                        SCM_EOL);
      if (scm_car (*tail) == SCM_UNSPECIFIED)
        return SCM_UNSPECIFIED;
      tail = SCM_CDRLOC (*tail);
    }

  return new_args;
}

SCM
evaluate_with_simple_closure (SCM delayed_argument,
                              SCM expr,
                              bool pure,
                              int start,
                              int end)
{
  if (Simple_closure *sc = Simple_closure::unsmob (expr))
    {
      SCM inside = sc->expression ();
      SCM proc = !pure && Unpure_pure_container::is_smob (scm_car (inside))
        ? Unpure_pure_container::unsmob (scm_car (inside))->unpure_part ()
        : scm_car (inside);
      SCM args = scm_cons (delayed_argument,
                           evaluate_args (delayed_argument, scm_cdr (inside),
                                          pure, start, end));
      if (scm_cdr (args) == SCM_UNSPECIFIED)
        return SCM_UNSPECIFIED;
      if (pure)
        return call_pure_function (proc, args, start, end);
      return scm_apply_0 (proc, args);
    }
  else if (!scm_is_pair (expr))
    return expr;
  else if (scm_car (expr) == ly_symbol2scm ("quote"))
    return scm_cadr (expr);
  else if (Unpure_pure_container::is_smob (scm_car (expr))
           || ly_is_procedure (scm_car (expr)))
    {
      SCM proc = !pure && Unpure_pure_container::is_smob (scm_car (expr))
        ? Unpure_pure_container::unsmob (scm_car (expr))->unpure_part ()
        : scm_car (expr);
      SCM args = evaluate_args (delayed_argument, scm_cdr (expr), pure, start, end);
      if (args == SCM_UNSPECIFIED)
        return SCM_UNSPECIFIED;
      if (pure)
        return call_pure_function (proc, args, start, end);
      return scm_apply_0 (proc, args);
    }
  else
    // ugh. deviation from standard. Should print error?
    return evaluate_args (delayed_argument, scm_cdr (expr), pure, start, end);

  assert (false);
  return SCM_EOL;
}

const char Simple_closure::type_p_name_[] = "ly:simple-closure?";

LY_DEFINE (ly_make_simple_closure, "ly:make-simple-closure",
           1, 0, 0, (SCM expr),
           "Make a simple closure.  @var{expr} should be form of"
           " @code{(@var{func} @var{a1} @var{a2} @dots{})}, and will be"
           " invoked as @code{(@var{func} @var{delayed-arg} @var{a1}"
           " @var{a2} @dots{})}.")
{
  return Simple_closure::make_smob (expr);
}

LY_DEFINE (ly_eval_simple_closure, "ly:eval-simple-closure",
           2, 2, 0, (SCM delayed, SCM closure, SCM scm_start, SCM scm_end),
           "Evaluate a simple @var{closure} with the given @var{delayed}"
           " argument.  If @var{scm-start} and @var{scm-end} are defined,"
           " evaluate it purely with those start and end points.")
{
  LY_ASSERT_SMOB (Simple_closure, closure, 2);
  bool pure = (scm_is_number (scm_start) && scm_is_number (scm_end));
  int start = robust_scm2int (scm_start, 0);
  int end = robust_scm2int (scm_end, 0);
  SCM expr = Simple_closure::unsmob (closure)->expression ();
  return evaluate_with_simple_closure (delayed, expr, pure, start, end);
}

int
Simple_closure::print_smob (SCM port, scm_print_state *)
{
  scm_puts ("#<simple-closure ", port);
  scm_display (expression (), port);
  scm_puts (" >", port);
  return 1;
}
