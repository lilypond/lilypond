/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "international.hh"
#include "lily-imports.hh"
#include "lily-lexer.hh"
#include "lily-parser.hh"
#include "overlay-string-port.hh"
#include "source-file.hh"

#include <cstdio>

// Catch stack traces on error.
bool parse_protect_global = true;

// Input to parsing and evaluation Scheme. We have to group these so
// we can pass them as a void* through GUILE.
struct Parse_start
{
  // Holds the SCM expression to be evaluated; unused for parsing.
  SCM form_;

  // Start of the to-be-parsed form.
  Input start_;

  // Output: full extent of the parsed form.
  Input parsed_;
  Lily_parser *parser_;

  Parse_start (SCM form, const Input &start, Lily_parser *parser)
    : form_ (form), start_ (start), parser_ (parser)
  {
  }

  static SCM handle_error (void *data, SCM /*tag*/, SCM args)
  {
    const auto *const ps = reinterpret_cast<Parse_start *> (data);

    ps->start_.non_fatal_error
    (_ ("GUILE signaled an error for the expression beginning here"));

    if (scm_ilength (args) > 2)
      scm_display_error_message (scm_cadr (args), scm_caddr (args), scm_current_error_port ());

    return SCM_UNSPECIFIED;
  }
};

// Pass string to scm parser, reading one expression.  Return result
// value. Parse_start::location_ is adjusted to cover the entire
// expression.
SCM
internal_parse_embedded_scheme (Parse_start *ps)
{
  // start_ is the first byte of the Scheme expression, ie. in
  // "... #(bla)", it is the offset of the '('
  const Input &start = ps->start_;
  ssize_t line_number, line_char, column, line_byte_offset;
  start.get_counts (&line_number, &line_char, &column, &line_byte_offset);

  ssize_t byte_offset = start.start () - start.get_source_file ()->c_str ();
  ssize_t length = static_cast<ssize_t> (start.get_source_file ()->length ());
  Overlay_string_port overlay (start.start (), length - byte_offset);

  SCM port = overlay.as_port ();
  scm_set_port_line_x (port, scm_from_ssize_t (line_number - 1));
  scm_set_port_filename_x (port, ly_string2scm (start.get_source_file ()->name_string ().c_str ()));
  // TODO: Do GUILE ports count in characters or bytes? Do they do tab
  // expansion for column counts?
  scm_set_port_column_x (port, scm_from_ssize_t (column - 1));

  bool multiple = '@' == *ps->start_.start ();
  if (multiple)
    (void) scm_read_char (port);

  SCM form = scm_read (port);
  ssize_t consumed = scm_to_ssize_t (scm_ftell (port));
  scm_close_port (port);

  ps->parsed_.set (start.get_source_file (), start.start (),
                   start.start () + consumed);

  if (SCM_EOF_OBJECT_P (form))
    return SCM_UNDEFINED;

  if (ps->parser_->lexer_->top_input ())
    {
      // Find any precompiled form.
      SCM c = scm_assv_ref (ps->parser_->closures_,
                            scm_from_ssize_t (byte_offset));
      if (scm_is_true (c))
        return c;
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
                             ps,
                             &Parse_start::handle_error, ps);
}

// Try parsing.  Upon failure return SCM_UNDEFINED. Upon success, set
// parsed_output to the cover the entire form. parsed_output may not
// be null.
SCM
parse_embedded_scheme (const Input &start, Lily_parser *parser, Input *parsed_output)
{
  Parse_start ps (SCM_UNDEFINED, start, parser);

  SCM result = parse_protect_global
               ? protected_parse_embedded_scheme (&ps)
               : internal_parse_embedded_scheme (&ps);

  *parsed_output = ps.parsed_;
  return result;
}

// EVALUATION

SCM
evaluate_scheme_form_void (void *p)
{
  Parse_start *ps = static_cast<Parse_start *> (p);
  // If ps->form_ is a procedure, it's a thunk returning the value(s)
  // for a Scheme expression inside #{ #}.
  if (ly_is_procedure (ps->form_))
    return ly_call (ps->form_);
  return scm_primitive_eval (ps->form_);
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
evaluate_embedded_scheme (SCM form, Input const &start, Lily_parser *parser)
{
  Parse_start ps (form, start, parser);

  // Establish the quasi-parameter (*location*) by using the location
  // `start` covering the input of the Scheme form during its
  // evaluation
  SCM ans = scm_c_with_fluid
            (Lily::f_location,
             start.smobbed_copy (),
             parse_protect_global ? protected_evaluate_scheme_form
             : evaluate_scheme_form_void, &ps);

  scm_remember_upto_here_1 (form);
  return ans;
}
