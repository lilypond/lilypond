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
#include "sources.hh"

#include <cstdio>
#include <cstdlib>

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
    : form_ (form),
      start_ (start),
      parser_ (parser)
  {
  }

  // The pre-unwind handler, which prints the Scheme error.
  static SCM handle_error_before_unwinding (void *data, SCM tag, SCM args)
  {
    const auto *const ps = reinterpret_cast<Parse_start *> (data);

    // Capture the call stack.
    SCM call_stack = scm_make_stack (SCM_BOOL_T, SCM_EOL);
    SCM error_source = SCM_BOOL_F;

    // Try to find back the cause of the error in the user's Scheme code.  If
    // user code called a function defined by Guile or LilyPond and this errors
    // out deep in the call stack, we'll report the error in the procedure where
    // the failure occurred, but the error message will be attached to the
    // initial call in the LilyPond file if possible.  We do this by walking
    // back in the call stack and finding the first frame that has one of the
    // LilyPond source files as its file name.
    SCM top_frame = SCM_BOOL_F;
    // It would be surprising if the stack were empty, but let's play it safe.
    if (from_scm<int> (scm_stack_length (call_stack)) == 0)
      {
        programming_error ("call stack empty while handling Guile error");
      }
    else
      {
        // Use scm_stack_ref to get the first frame, but not the following ones
        // because that would be quadratic (scm_stack_ref is linear in its
        // second argument like list-ref).
        top_frame = scm_stack_ref (call_stack, to_scm (0));
        SCM search_frame = top_frame;
        SCM source_files = ly_source_files (/* parser */ SCM_UNDEFINED);
        while (true)
          {
            SCM source = scm_frame_source (search_frame);
            if (scm_is_pair (source))
              {
                SCM filename = scm_cadr (source);
                // This catches user .ly files, but not init files.
                if (scm_is_true (scm_member (filename, source_files))
                    // ugh
                    && !ly_is_equal (filename,
                                     ly_string2scm ("<included string>"))
                    // ly:parser-include-string uses "<included string>" but
                    // ly:parser-parse-string uses "<string>" (should this be
                    // harmonized?).
                    && !ly_is_equal (filename, ly_string2scm ("<string>")))
                  {
                    error_source = source;
                    break;
                  }
              }
            search_frame = scm_frame_previous (search_frame);
            if (scm_is_false (search_frame)) // end of stack
              break;
          }
      }

    if (scm_is_false (error_source))
      {
        // No location found.  This can happen for syntax errors.  Use the start
        // of the Scheme expression where the error was raised.
        ps->start_.non_fatal_error (
          _ ("Guile signaled an error for the expression beginning here"));
      }
    else
      {
        SCM filename = scm_cadr (error_source);
        SCM line = scm_caddr (error_source);
        SCM column = scm_cdddr (error_source);
        // We need to find back the line in the .ly file.  This may look a bit
        // clumsy, but it turns out to be the easiest way not to use the
        // Source_file infrastructure here, because the parser is oriented
        // towards getting line/column info from the current char* position (to
        // work with Flex), whereas here we have the line/column and we want to
        // get the line from them.
        SCM port
          = scm_open_file_with_encoding (filename, ly_string2scm ("r"),
                                         SCM_BOOL_F, // don't guess encoding
                                         ly_string2scm ("UTF8"));
        // Wait until the relevant line
        while (!ly_is_eqv (scm_port_line (port), line))
          (void) scm_read_line (port);
        // It looks like we could just use scm_substring, but we can't, because
        // of tab expansion, which Guile does on port columns and thus factors
        // into the column that the error gets.  We do it in a way that
        // guarantees correctness without having to do the expansion ourselves.
        SCM before_chars = SCM_EOL;
        while (!ly_is_eqv (scm_port_column (port), column))
          before_chars = scm_cons (scm_read_char (port), before_chars);
        SCM before_substring
          = scm_string (scm_reverse_x (before_chars, SCM_EOL));
        SCM after_substring = scm_car (scm_read_line (port));
        static SCM space = scm_integer_to_char (to_scm (32));
        // Note that we get the "cutting point" right wrt. tab expansion, but we
        // don't care to make the "before" and "after" part align at the cutting
        // point, which is not really possible anyway because there is no
        // universal tab width.
        SCM context = scm_make_string (column, space);
        non_fatal_error (
          _ ("Guile signaled an error for the expression beginning here") + "\n"
            + ly_scm2string (before_substring) + "\n" + ly_scm2string (context)
            + ly_scm2string (after_substring),
          ly_scm2string (filename) + ":"
            + ly_scm_write_string (scm_oneplus (line)) + ":"
            + ly_scm_write_string (scm_oneplus (column)));
      }

    // If enabled, print a backtrace.  "enabled" means that Guile would print a
    // backtrace if the error were not handled.  This can be turned on with
    // #(debug-enable 'backtrace) or by running with -ddebug-eval.
    if (scm_is_true (
          scm_memq (ly_symbol2scm ("backtrace"), Guile_user::debug_options ())))
      {
        // Use scm_display_backtrace and not the scm_backtrace convenience
        // wrapper because the latter outputs to stdout whereas we want stderr.
        scm_display_backtrace (call_stack, scm_current_error_port (),
                               SCM_BOOL_F, // don't cut inner frames
                               SCM_BOOL_F  // don't cut outer frames
        );
      }

    // Now let Guile tell us what the error is about.  We pass #f for the
    // "frame" argument here since we already showed where the error was
    // ourselves.
    scm_print_exception (scm_current_error_port (), SCM_BOOL_F, tag, args);

    // In -dno-protected-scheme-parsing mode, we abort compilation entirely.
    // Note that even in this mode, we do error handling and don't just "run the
    // code without catch", because we still want more helpful backtraces for
    // any errors, e.g., the error location in the LilyPond file and not on the
    // line of code in one of LilyPond's .scm files that the Scheme code in the
    // .ly file called.
    if (!parse_protect_global)
      exit (1);

    // This *unspecified* is unimportant.
    return SCM_UNSPECIFIED;
  }

  // The outer handler, which just specifies that any Scheme expression whose
  // evaluation resulted in an error is evaluated as *unspecified* in order to
  // be able to continue compiling the main LilyPond file.  (Unreachable in
  // -dno-protected-scheme-parsing.)
  static SCM handle_error_after_unwinding (void * /*data*/, SCM /*tag*/,
                                           SCM /*args*/)
  {
    // This *unspecified* is important, it's the value returned to LilyPond.
    return SCM_UNSPECIFIED;
  }
};

// PARSING

// Pass string to scm parser, reading one expression.  Return result
// value. Parse_start::location_ is adjusted to cover the entire
// expression.
SCM
internal_parse_embedded_scheme (void *p)
{
  Parse_start *ps = static_cast<Parse_start *> (p);

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
  scm_set_port_filename_x (
    port, ly_string2scm (start.get_source_file ()->name_string ().c_str ()));
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
      SCM c
        = scm_assv_ref (ps->parser_->closures_, scm_from_ssize_t (byte_offset));
      if (scm_is_true (c))
        return c;
    }

  if (multiple)
    form = ly_list (ly_symbol2scm ("apply"), ly_symbol2scm ("values"), form);
  return form;
}

// Try parsing.  Upon failure return SCM_UNDEFINED. Upon success, set
// parsed_output to the cover the entire form. parsed_output may not
// be null.
SCM
parse_embedded_scheme (const Input &start, Lily_parser *parser,
                       Input *parsed_output)
{
  Parse_start ps (SCM_UNDEFINED, start, parser);

  // Catch #t : catch all Scheme level errors.
  SCM result = scm_c_catch (SCM_BOOL_T, internal_parse_embedded_scheme, &ps,
                            &Parse_start::handle_error_after_unwinding, &ps,
                            &Parse_start::handle_error_before_unwinding, &ps);

  *parsed_output = ps.parsed_;
  return result;
}

// EVALUATION

SCM
internal_evaluate_embedded_scheme (void *p)
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
  bool compile
    = (from_scm<bool> (ly_get_option (ly_symbol2scm ("compile-scheme-code")))
       && ps->parser_->lexer_->is_main_input_
       // Avoid compilation overhead for trivial expressions.
       && !scm_is_number (ps->form_) && !scm_is_string (ps->form_)
       && !scm_is_bool (ps->form_) && !scm_is_keyword (ps->form_)
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
  SCM bytecode = Compile::compile (
    ps->form_, ly_keyword2scm ("to"), ly_symbol2scm ("bytecode"),
    ly_keyword2scm ("env"), scm_current_module (),
    // Turn off optimizations, they make for very slow compilation.
    ly_keyword2scm ("optimization-level"), to_scm (0));
#else
  SCM bytecode = Compile::compile (
    ps->form_, ly_keyword2scm ("to"), ly_symbol2scm ("bytecode"),
    ly_keyword2scm ("env"), scm_current_module (),
    // To turn off optimizations, reuse the options that LilyPond
    // uses when compiling its own .scm files.
    ly_keyword2scm ("opts"), Guile_user::p_auto_compilation_options);
#endif
  scm_set_current_warning_port (port);
  SCM thunk = Loader::load_thunk_from_memory (bytecode);
  return ly_call (thunk);
}

SCM
evaluate_embedded_scheme (SCM form, Input const &start, Lily_parser *parser)
{
  Parse_start ps (form, start, parser);

  // Establish the quasi-parameter (*location*) by using the location `start`
  // covering the input of the Scheme form during its evaluation
  scm_dynwind_begin (SCM_F_DYNWIND_REWINDABLE);
  scm_dynwind_fluid (Lily::f_location, start.smobbed_copy ());

  SCM result = scm_c_catch (SCM_BOOL_T, internal_evaluate_embedded_scheme, &ps,
                            &Parse_start::handle_error_after_unwinding, &ps,
                            &Parse_start::handle_error_before_unwinding, &ps);

  scm_dynwind_end ();

  scm_remember_upto_here_1 (form);
  return result;
}
