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
#include "program-option.hh"
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
    form = ly_list (ly_symbol2scm ("apply"), ly_symbol2scm ("values"), form);
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
  // Else, it's a Scheme form.  Run it.  Guile 2+ has two methods for turning
  // Scheme forms into values, evaluation via eval, or compilation via compile.
  // Compiling yields faster code, but the compilation process itself is slower,
  // so for running a one-off Scheme form, eval would be faster.  However, there
  // is an additional consideration at play: eval yields horrible error
  // messages.  For this reason, code coming from the user can be optionally
  // compiled using the -dcompile-scheme-code option.  It is not turned on by
  // default due to a limitation in the byte-compiler that prevents compiling
  // more than a few thousand expressions, which stops us from using in make
  // check.  See
  // https://lists.gnu.org/archive/html/guile-devel/2022-08/msg00033.html.  For
  // the same reason, we don't compile code from the init files for now, only
  // code from the user.
  bool compile = (from_scm<bool> (ly_get_option (ly_symbol2scm ("compile-scheme-code")))
                  && ps->parser_->lexer_->is_main_input_
                  // Avoid compilation overhead for trivial expressions.
                  && !scm_is_number (ps->form_)
                  && !scm_is_string (ps->form_)
                  && !scm_is_bool (ps->form_)
                  && !scm_is_keyword (ps->form_)
                  && !(scm_is_pair (ps->form_)
                       && scm_is_eq (scm_car (ps->form_), ly_symbol2scm ("quote"))));
  if (!compile)
    {
      // The simple case: evaluation.
      return scm_primitive_eval (ps->form_);
    }
  // The complex case: compilation.  We have to do something a bit
  // awkward here.  This should really be
  //
  //   return Compile::compile (form, options ...)
  //
  // but compile has a bug: it discards multiple values, as happens
  // for example if #@ or $@ is used.  This is
  // https://debbugs.gnu.org/cgi/bugreport.cgi?bug=57123.  For this
  // reason, we need to mimic its implementation in a way that is
  // multiple-values-proof: instead of
  //
  //  (compile form options ...)
  //
  // we do
  //
  //  ((load-thunk-from-memory (compile form #:to 'bytecode options ...)))
  //
  // Namely, we only compile to bytecode, and do the conversion from
  // bytecode to value ourselves by loading the thunk from bytecode
  // and calling it.  (We would have to use save-module-excursion
  // while calling if we needed to compile in a module other than the
  // current module, but this is not the case.)
  //
  // Furthermore, Guile's warning handling is unfortunately buggy as
  // well, https://debbugs.gnu.org/cgi/bugreport.cgi?bug=57119.  For
  // this reason, we rebind the warning port while compiling to ensure
  // no compilation warning ever reaches us. (Guile's compilation
  // warnings are usually noise.)
  SCM port = scm_current_warning_port ();
  static SCM devnull = scm_sys_make_void_port (ly_string2scm ("w"));
  scm_set_current_warning_port (devnull);
  #if SCM_MAJOR_VERSION >= 3
  SCM bytecode =
    Compile::compile (
      ps->form_,
      ly_keyword2scm ("to"), ly_symbol2scm ("bytecode"),
      ly_keyword2scm ("env"), scm_current_module (),
      // Turn off optimizations, they make for very slow compilation.
      ly_keyword2scm ("optimization-level"), to_scm (0)
    );
  #elif SCM_MAJOR_VERSION == 2
  SCM bytecode =
    Compile::compile (
      ps->form_,
      ly_keyword2scm ("to"), ly_symbol2scm ("bytecode"),
      ly_keyword2scm ("env"), scm_current_module (),
      // To turn off optimizations, reuse the options that LilyPond
      // uses when compiling its own .scm files.
      ly_keyword2scm ("opts"), Guile_user::p_auto_compilation_options
    );
  #endif
  scm_set_current_warning_port (port);
  SCM thunk = Loader::load_thunk_from_memory (bytecode);
  return ly_call (thunk);
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
