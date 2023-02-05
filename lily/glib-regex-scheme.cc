/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2022-2022 Jean Abou Samra <jean@abou-samra.fr>

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

// Scheme interface to the GLib regex API (based on PCRE).  This is a bit sad:
// Guile also has regular expressions, but since they are merely a wrapper
// around the system C library's regex interface, their behavior is
// locale-dependent (https://debbugs.gnu.org/cgi/bugreport.cgi?bug=57507).  In a
// non-Unicode-aware locale, they won't work on Unicode data. For this reason,
// we don't want to use them.  In contrast, the GLib functions are
// platform-independent and locale-independent.  They expect UTF-8 data.  The
// regular expression syntax they allow is also richer.

// This interface has not been designed for use from C++ because there is little
// need at the time of this writing and because C++ code can simply use GLib
// directly.

// It follows the Guile interface where it makes sense, and deviates from it
// where it is suboptimal.  For example, ly:regex-replace is the analogue of
// regexp-substitute/global, but always outputs to a string, and does not
// require starting the replacements with 'pre and ending them with 'post.
// Also, there is no equivalent of string-match: one must use ly:make-regex +
// ly:regex-exec, which encourages storing the regex object in a variable in
// order to compile it only once.

#include "glib-utils.hh"
#include "lily-guile.hh"
#include "lily-imports.hh"
#include "ly-scm-list.hh"
#include "smobs.hh"

#include "glib.h"

#include <cassert>
#include <cstdlib>
#include <memory>

// For readability
const int NULL_TERMINATED_STRING = -1;
const int START_AT_BEGINNING = 0;
// G_REGEX_DEFAULT and G_REGEX_MATCH_DEFAULT require GLib 2.74.
const auto DEFAULT_COMPILE_FLAGS = static_cast<GRegexCompileFlags> (0);
const auto DEFAULT_MATCH_FLAGS = static_cast<GRegexMatchFlags> (0);

// A smob that just owns a pointer to a GLib regex object.

class Regex : public Smob<Regex>
{
public:
  static const char *const type_p_name_;
  Regex (GRegex *regex);
  virtual ~Regex ();
  GRegex *regex () { return regex_; }

private:
  GRegex *regex_;
};

const char *const Regex::type_p_name_ = "ly:regex?";

static void
check_error (GError *error, const char *func_name)
{
  if (error)
    {
      scm_misc_error (func_name, "the GLib regex engine signaled an error: ~a",
                      ly_list (ly_string2scm (error->message)));
    }
}

Regex::Regex (GRegex *regex)
  : regex_ (regex)
{
  smobify_self ();
}

Regex::~Regex ()
{
  g_regex_unref (regex_);
}

LY_DEFINE (ly_make_regex, "ly:make-regex", 1, 0, 0, (SCM pattern),
           R"(
Construct a new regular expression object.

Note that regular expressions created with this function are distinct from Guile
native regular expressions (the latter don't fully support Unicode). They should
be used with @code{ly:regex-@dots{}} functions.

The full reference for the supported regular expression syntax
can be read at
@uref{https://www.pcre.org/original/doc/html/pcrepattern.html}.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, pattern, 1);
  unique_stdlib_ptr<char> p = ly_scm2str0 (pattern);
  GError *error = nullptr;
  GRegex *regex = g_regex_new (p.get (), DEFAULT_COMPILE_FLAGS,
                               DEFAULT_MATCH_FLAGS, &error);
  check_error (error, "ly:make-regex");
  return (new Regex (regex))->unprotect ();
}

LY_DEFINE (ly_regex_quote, "ly:regex-quote", 1, 0, 0, (SCM string),
           R"delim(
Escape special characters in @var{string}, forming a regular
expression pattern that matches exactly @var{string}.

Example:

@example
(ly:regex-quote "$2")
@result{} "\\$2"
@end example
           )delim")
{
  LY_ASSERT_TYPE (scm_is_string, string, 1);
  unique_stdlib_ptr<char> s = ly_scm2str0 (string);
  unique_glib_ptr<char> result (
    g_regex_escape_string (s.get (), NULL_TERMINATED_STRING));
  return scm_from_utf8_string (result.get ());
}

// GLib functions return positions in bytes, not in characters.  This function
// modifies pos, given in bytes, so that it is given in characters after the
// call.  It assumes that prev_bytes in bytes is prev_chars in characters; this
// allows to skip the start of the string until prev_bytes bytes.  It updates
// prev_bytes and prev_chars to provide a reference for the next call.  In this
// way, we avoid quadratic processing when replacing in a large string.
static void
fix_up_position (const char *str, vsize *pos, vsize *prev_bytes,
                 vsize *prev_chars)
{
  for (; *prev_bytes < *pos; (*prev_bytes)++)
    if ((str[*prev_bytes] & 0xc0) != 0x80) // skip UTF-8 continuation bytes
      (*prev_chars)++;
  for (; *prev_bytes > *pos; (*prev_bytes)--)
    if ((str[*prev_bytes - 1] & 0xc0) != 0x80)
      (*prev_chars)--;
  *pos = *prev_chars; // make pos expressed in chars
}

static SCM
match_info2scm (SCM original_string, const char *str0,
                const GMatchInfo *match_info, vsize *prev_bytes,
                vsize *prev_chars)
{
  // The number of capturing groups.  Add 1 since we use 0 for the whole regex.
  // Importantly, this may differ from g_match_info_get_match_count: the latter
  // can skip groups at the end if they didn't match anything (e.g., the second
  // group when matching the string "aa" against the regex "(a+)|(b+)").
  GRegex *original_regex = g_match_info_get_regex (match_info);
  vsize count = g_regex_get_capture_count (original_regex) + 1;
  SCM positions = scm_c_make_vector (count, SCM_UNDEFINED);
  for (vsize i = 0; i < count; i++)
    {
      int signed_start;
      int signed_end;
      SCM pos;
      bool fetched = g_match_info_fetch_pos (match_info, static_cast<int> (i),
                                             &signed_start, &signed_end);
      // This condition means that the group was not considered, see the
      // remark above.
      if (!fetched || (signed_start == -1 && signed_end == -1))
        {
          pos = SCM_BOOL_F;
        }
      else
        {
          vsize start = static_cast<vsize> (signed_start);
          vsize end = static_cast<vsize> (signed_end);
          fix_up_position (str0, &start, prev_bytes, prev_chars);
          fix_up_position (str0, &end, prev_bytes, prev_chars);
          pos = scm_cons (to_scm (start), to_scm (end));
        }
      scm_c_vector_set_x (positions, i, pos);
    }
  return Goops::make (Lily::regex_match_class,
                      ly_keyword2scm ("original-string"), original_string,
                      ly_keyword2scm ("substring-positions"), positions);
}

LY_DEFINE (ly_regex_exec, "ly:regex-exec", 2, 0, 0, (SCM regex, SCM string),
           R"delim(
Scan @var{string} for a match of the regular expression object
@var{regex} (constructed with @code{ly:make-regex}).  Return
a match object or @code{#f}.  See @code{ly:regex-match-@dots{}}
functions for what you can do with the match object.

For example, this extracts the components of a date in YYYY-MM-DD format:

@quotation
@verbatim
#(define date-components
   (let ((date-regex (ly:make-regex "^(\\d{4})-(\\d{2})-(\\d{2})$")))
     (lambda (date)
       (let ((match (ly:regex-exec date-regex date)))
         (if match
             (list (string->number (ly:regex-match-substring match 1))
                   (string->number (ly:regex-match-substring match 2))
                   (string->number (ly:regex-match-substring match 3)))
             (error "not a date"))))))
@end verbatim
@end quotation
           )delim")
{
  Regex *r = LY_ASSERT_SMOB (Regex, regex, 1);
  LY_ASSERT_TYPE (scm_is_string, string, 2);
  unique_stdlib_ptr<char> s = ly_scm2str0 (string);
  GError *error = nullptr;
  GMatchInfo *match_info = nullptr;
  bool matched = g_regex_match_full (r->regex (), s.get (),
                                     NULL_TERMINATED_STRING, START_AT_BEGINNING,
                                     DEFAULT_MATCH_FLAGS, &match_info, &error);
  // Make sure r->regex () is not freed while matching
  scm_remember_upto_here (regex);
  check_error (error, "ly:regex-exec");
  SCM ret;
  if (matched)
    {
      vsize prev_bytes = 0;
      vsize prev_chars = 0;
      ret = match_info2scm (string, s.get (), match_info, &prev_bytes,
                            &prev_chars);
    }
  else
    {
      ret = SCM_BOOL_F;
    }
  // According to the GLib documentation, the GMatchInfo object must be freed
  // even if the string doesn't match.
  g_match_info_unref (match_info);
  return ret;
}

LY_DEFINE (ly_regex_exec_2_list, "ly:regex-exec->list", 2, 0, 0,
           (SCM regex, SCM string),
           R"(
Like @code{ly:regex-exec}, but return a list of non-overlapping matches
instead of the first match only."
           )")
{
  Regex *r = LY_ASSERT_SMOB (Regex, regex, 1);
  LY_ASSERT_TYPE (scm_is_string, string, 2);
  unique_stdlib_ptr<char> s = ly_scm2str0 (string);
  GError *error = nullptr;
  GMatchInfo *match_info = nullptr;
  bool matched = g_regex_match_full (r->regex (), s.get (),
                                     NULL_TERMINATED_STRING, START_AT_BEGINNING,
                                     DEFAULT_MATCH_FLAGS, &match_info, &error);
  // Make sure r->regex () is not freed while matching.
  scm_remember_upto_here (regex);
  check_error (error, "ly:regex-exec->list");
  SCM ret;
  if (matched)
    {
      vsize prev_bytes = 0;
      vsize prev_chars = 0;
      ret = ly_list (match_info2scm (string, s.get (), match_info, &prev_bytes,
                                     &prev_chars));
      while (true)
        {
          bool new_match = g_match_info_next (match_info, &error);
          check_error (error, "ly:regex-exec->list");
          if (new_match)
            {
              ret = scm_cons (match_info2scm (string, s.get (), match_info,
                                              &prev_bytes, &prev_chars),
                              ret);
            }
          else
            {
              break;
            }
        }
      ret = scm_reverse_x (ret, SCM_EOL);
    }
  else
    {
      ret = SCM_EOL;
    }
  g_match_info_unref (match_info);
  return ret;
}

struct Replacement_data
{
  // SCMs are protected here because this struct is allocated on the stack.
  SCM original_string_;
  const char *original_str0_;
  SCM replacements_;
  vsize prev_bytes = 0;
  vsize prev_chars = 0;
  Replacement_data (SCM orig, const char *orig_str0, SCM repls)
    : original_string_ (orig),
      original_str0_ (orig_str0),
      replacements_ (repls) {};
};

static int
replacement_callback (const GMatchInfo *match_info, GString *res,
                      void *replacement_data_void)
{
  Replacement_data *replacement_data
    = static_cast<Replacement_data *> (replacement_data_void);
  for (SCM repl : as_ly_scm_list (replacement_data->replacements_))
    {
      if (scm_is_string (repl))
        {
          unique_stdlib_ptr<char> repl_str0 = ly_scm2str0 (repl);
          g_string_append (res, repl_str0.get ());
        }
      else if (is_scm<vsize> (repl))
        {
          int group_index = from_scm<int> (repl);
          unique_glib_ptr<char> group (
            g_match_info_fetch (match_info, group_index));
          g_string_append (res, group.get ());
        }
      else
        {
          assert (ly_is_procedure (repl));
          SCM scm_match_info = match_info2scm (
            replacement_data->original_string_,
            replacement_data->original_str0_, match_info,
            &replacement_data->prev_bytes, &replacement_data->prev_chars);
          SCM repl_string = ly_call (repl, scm_match_info);
          LY_ASSERT_TYPE (scm_is_string, repl_string, 0);
          unique_stdlib_ptr<char> repl_str0 = ly_scm2str0 (repl_string);
          g_string_append (res, repl_str0.get ());
        }
    }
  return 0; // continue replacing
}

LY_DEFINE (ly_regex_replace, "ly:regex-replace", 2, 0, 1,
           (SCM regex, SCM string, SCM replacements),
           R"delim(
Scan for matches of the compiled regular expression @var{regex}
(created with @code{ly:make-regex}) in the string @var{string},
and form a new string by replacing them according to the @var{replacements}.
Each replacement argument can be:

@itemize
@item
A string, which is output as-is.

@item
A non-negative integer, which is interpreted as a match substring
index (see @code{ly:regex-match-substring}.

@item
A procedure, which is called on the match object, and should return
a string.
@end itemize

This example converts a date from YYYY-MM-DD format to DD-MM-YYYY format:

@quotation
@verbatim
#(define date-yyyy-mm-dd->dd-mm-yyyy
   (let ((date-regex (ly:make-regex "(\\d{4})-(\\d{2})-(\\d{2})")))
     (lambda (date)
       (ly:regex-replace date-regex date 3 "-" 2 "-" 1))))
@end verbatim
@end quotation

This example does the same, using a procedure:

@quotation
@verbatim
#(define date-yyyy-mm-dd->dd-mm-yyyy
   (let ((date-regex (ly:make-regex "(\\d{4})-(\\d{2})-(\\d{2})")))
     (lambda (date)
       (ly:regex-replace
        date-regex
        date
        (lambda (match)
          (format #f "~a-~a-~a"
                  (ly:regex-match-substring match 3)
                  (ly:regex-match-substring match 2)
                  (ly:regex-match-substring match 1)))))))
@end verbatim
@end quotation
           )delim")
{
  Regex *r = LY_ASSERT_SMOB (Regex, regex, 1);
  LY_ASSERT_TYPE (scm_is_string, string, 2);
  unique_stdlib_ptr<char> s = ly_scm2str0 (string);
  int arg_idx = 3;
  for (SCM repl : as_ly_scm_list (replacements))
    {
      if (!(scm_is_string (repl) || is_scm<vsize> (repl)
            || ly_is_procedure (repl)))
        {
          scm_wrong_type_arg_msg ("ly:regex-replace", arg_idx, repl,
                                  "string, non-negative integer or procedure");
        }
      arg_idx++;
    }
  Replacement_data replacement_data (string, s.get (), replacements);
  GError *error = nullptr;
  unique_glib_ptr<char> result (g_regex_replace_eval (
    r->regex (), s.get (), NULL_TERMINATED_STRING, START_AT_BEGINNING,
    DEFAULT_MATCH_FLAGS, static_cast<GRegexEvalCallback> (replacement_callback),
    &replacement_data, &error));
  check_error (error, "ly:regex-replace");
  scm_remember_upto_here (regex);
  return scm_from_utf8_string (result.get ());
}
