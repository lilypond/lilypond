/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "parse-scm.hh"

#include <cstdio>
using namespace std;

#include "lily-parser.hh"
#include "lily-lexer.hh"
#include "international.hh"
#include "main.hh"
#include "paper-book.hh"
#include "source-file.hh"

/* Pass string to scm parser, read one expression.
   Return result value and #chars read.

   Thanks to Gary Houston <ghouston@freewire.co.uk>  */
SCM
internal_ly_parse_scm (Parse_start *ps)
{
  Input &hi = ps->location_;
  Source_file *sf = hi.get_source_file ();
  SCM port = sf->get_port ();

  long off = hi.start () - sf->c_str ();

  scm_seek (port, scm_from_long (off), scm_from_long (SEEK_SET));
  SCM from = scm_ftell (port);

  scm_set_port_line_x (port, scm_from_int (hi.line_number () - 1));
  scm_set_port_column_x (port, scm_from_int (hi.column_number () - 1));

  bool multiple = ly_is_equal (scm_peek_char (port), SCM_MAKE_CHAR ('@'));

  if (multiple)
    (void) scm_read_char (port);

  SCM form = scm_read (port);
  SCM to = scm_ftell (port);

  hi.set (hi.get_source_file (),
          hi.start (),
          hi.start () + scm_to_int (scm_difference (to, from)));

  if (!SCM_EOF_OBJECT_P (form))
    {
      if (ps->parser_->lexer_->top_input ())
        {
          // Find any precompiled form.
          SCM c = scm_assv_ref (ps->parser_->closures_, from);
          if (scm_is_true (c))
            // Replace form with a call to previously compiled closure
            form = scm_list_1 (c);
        }
      if (multiple)
        form = scm_list_3 (ly_symbol2scm ("apply"),
                           ly_symbol2scm ("values"),
                           form);
      return form;
    }

  /* Don't close the port here; if we re-enter this function via a
     continuation, then the next time we enter it, we'll get an error.
     It's a string port anyway, so there's no advantage to closing it
     early. */
  // scm_close_port (port);

  return SCM_UNDEFINED;
}

SCM
internal_ly_eval_scm (Parse_start *ps)
{
  if (ps->safe_)
    {
      static SCM module = SCM_BOOL_F;
      if (scm_is_false (module))
        {
          SCM function = ly_lily_module_constant ("make-safe-lilypond-module");
          module = scm_gc_protect_object (scm_call_0 (function));
        }

      // We define the parser so trusted Scheme functions can
      // access the real namespace underlying the parser.
      if (ps->parser_)
        scm_module_define (module, ly_symbol2scm ("parser"),
                           ps->parser_->self_scm ());
      return scm_eval (ps->form_, module);
    }
  return scm_primitive_eval (ps->form_);
}

SCM
catch_protected_parse_body (void *p)
{
  return internal_ly_parse_scm (static_cast<Parse_start *> (p));
}

SCM
catch_protected_eval_body (void *p)
{
  return internal_ly_eval_scm (static_cast<Parse_start *> (p));
}

SCM
parse_handler (void *data, SCM /*tag*/, SCM args)
{
  Parse_start *ps = (Parse_start *) data;

  ps->location_.error (_ ("GUILE signaled an error for the expression beginning here"));

  if (scm_ilength (args) > 2)
    scm_display_error_message (scm_cadr (args), scm_caddr (args), scm_current_error_port ());

  return SCM_UNDEFINED;
}

SCM
protected_ly_parse_scm (Parse_start *ps)
{
  /*
    Catch #t : catch all Scheme level errors.
   */
  return scm_internal_catch (SCM_BOOL_T,
                             catch_protected_parse_body,
                             (void *) ps,
                             &parse_handler, (void *) ps);
}

SCM
protected_ly_eval_scm (Parse_start *ps)
{
  /*
    Catch #t : catch all Scheme level errors.
   */
  return scm_internal_catch (SCM_BOOL_T,
                             catch_protected_eval_body,
                             (void *) ps,
                             &parse_handler, (void *) ps);
}

bool parse_protect_global = true;
bool parsed_objects_should_be_dead = false;

/* Try parsing.  Upon failure return SCM_UNDEFINED. */

SCM
ly_parse_scm (Input &i, bool safe, Lily_parser *parser)
{
  Parse_start ps (SCM_UNDEFINED, i, safe, parser);

  SCM ans = parse_protect_global ? protected_ly_parse_scm (&ps)
            : internal_ly_parse_scm (&ps);

  return ans;
}

SCM
ly_eval_scm (SCM form, Input i, bool safe, Lily_parser *parser)
{
  Parse_start ps (form, i, safe, parser);

  SCM ans = parse_protect_global ? protected_ly_eval_scm (&ps)
            : internal_ly_eval_scm (&ps);
  scm_remember_upto_here_1 (form);
  return ans;
}
