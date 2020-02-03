/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "international.hh"
#include "lily-imports.hh"
#include "lily-lexer.hh"
#include "lily-parser.hh"
#include "main.hh"
#include "overlay-string-port.hh"
#include "paper-book.hh"
#include "source-file.hh"

// Catch stack traces on error.
bool parse_protect_global = true;


// Input to parsing and evaluation Scheme. We have to group these so
// we can pass them as a void* through GUILE.
struct Parse_start
{
  // Holds the SCM expression to be evaluated; unused for parsing.
  SCM form_;
  Input *location_;
  bool safe_;
  Lily_parser *parser_;

  Parse_start (SCM form, Input *location, bool safe, Lily_parser *parser) :
    form_ (form), location_ (location), safe_ (safe), parser_ (parser)
  {
  }

  static SCM handle_error(void *data, SCM /*tag*/, SCM args)
  {
    Parse_start *ps = (Parse_start *) data;
    
    ps->location_->non_fatal_error
      (_ ("GUILE signaled an error for the expression beginning here"));
    
    if (scm_ilength (args) > 2)
      scm_display_error_message (scm_cadr (args), scm_caddr (args), scm_current_error_port ());
    
    return SCM_UNDEFINED;
  }
};

// Pass string to scm parser, reading one expression.  Return result
// value. Parse_start::location_ is adjusted to cover the entire
// expression.
SCM
internal_parse_embedded_scheme (Parse_start *ps)
{
  Input *loc = ps->location_;
  ssize_t line_number, line_char, column, byte_offset;
  loc->get_source_file ()->get_counts (loc->start (), &line_number, &line_char,
                                       &column, &byte_offset);

  Overlay_string_port overlay (loc->start (), loc->get_source_file ()->length ()
                                                  - byte_offset);

  SCM port = overlay.as_port ();
  scm_set_port_line_x (port, scm_from_ssize_t (line_number - 1));
  scm_set_port_column_x (port, scm_from_ssize_t (column - 1));

  bool multiple = '@' == *ps->location_->start ();
  if (multiple)
    (void) scm_read_char (port);

  SCM form = scm_read (port);
  ssize_t consumed = scm_to_ssize_t (scm_ftell (port));
  scm_close_port (port);
  loc->set (loc->get_source_file (), loc->start (), loc->start () + consumed);

  if (SCM_EOF_OBJECT_P (form))
    return SCM_UNDEFINED;

  if (ps->parser_->lexer_->top_input ())
    {
      // Find any precompiled form.
      SCM c = scm_assv_ref (ps->parser_->closures_,
                            scm_from_ssize_t (byte_offset));
      if (scm_is_true (c))
        // Replace form with a call to previously compiled closure
        form = scm_list_1 (c);
    }
  if (multiple)
    form = scm_list_3 (ly_symbol2scm ("apply"), ly_symbol2scm ("values"), form);
  return form;
}

SCM
parse_embedded_scheme_void (void *p)
{
  return internal_parse_embedded_scheme (static_cast<Parse_start *> (p));
}

SCM
protected_parse_embedded_scheme (Parse_start *ps)
{
  // Catch #t : catch all Scheme level errors.
  return scm_internal_catch (SCM_BOOL_T,
                             parse_embedded_scheme_void,
                             (void *) ps,
                             &Parse_start::handle_error, (void *) ps);
}

// Try parsing.  Upon failure return SCM_UNDEFINED.
SCM
parse_embedded_scheme (Input *i, bool safe, Lily_parser *parser)
{
  Parse_start ps (SCM_UNDEFINED, i, safe, parser);

  return parse_protect_global
      ? protected_parse_embedded_scheme (&ps)
      : internal_parse_embedded_scheme (&ps);
}

// EVALUATION

SCM
evaluate_scheme_form (Parse_start *ps)
{
  if (ps->safe_)
    {
      static SCM module = SCM_BOOL_F;
      if (scm_is_false (module))
        {
          module = scm_gc_protect_object (Lily::make_safe_lilypond_module ());
        }

      return scm_eval (ps->form_, module);
    }
  return scm_primitive_eval (ps->form_);
}


SCM
evaluate_scheme_form_void (void *p)
{
  return evaluate_scheme_form (static_cast<Parse_start *> (p));
}

SCM
protected_evaluate_scheme_form (void *ps)
{
  /*
    Catch #t : catch all Scheme level errors.
   */
  return scm_internal_catch (SCM_BOOL_T,
                             evaluate_scheme_form_void,
                             ps,
                             &Parse_start::handle_error, ps);
}

SCM
evaluate_embedded_scheme (SCM form, Input i, bool safe, Lily_parser *parser)
{
  Parse_start ps (form, &i, safe, parser);

  SCM ans = scm_c_with_fluid
    (Lily::f_location,
     i.smobbed_copy (),
     parse_protect_global ? protected_evaluate_scheme_form
     : evaluate_scheme_form_void, (void *) &ps);

  scm_remember_upto_here_1 (form);
  return ans;
}
