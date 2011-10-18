/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2011  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "program-option.hh"

#include <cstdio>
#include <cstring>
using namespace std;

#include "profile.hh"
#include "international.hh"
#include "main.hh"
#include "parse-scm.hh"
#include "string-convert.hh"
#include "warn.hh"

bool debug_skylines;
bool debug_property_callbacks;
bool debug_page_breaking_scoring;

bool music_strings_to_paths;
bool relative_includes;

/*
  Backwards compatibility.
*/
bool lily_1_8_relative = false;
bool lily_1_8_compatibility_used = false;
bool profile_property_accesses = false;
/*
  crash if internally the wrong type is used for a grob property.
*/
bool do_internal_type_checking_global;
bool strict_infinity_checking = false;

static SCM option_hash;

void
internal_set_option (SCM var,
                     SCM val)
{
  string varstr = robust_symbol2string (var, "");
  bool valbool = to_boolean (val);
  SCM val_scm_bool = scm_from_bool (valbool);
  if (0)
    ;
  else if (varstr == "profile-property-accesses")
    {
      profile_property_accesses = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "point-and-click")
    {
      point_and_click_global = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "protected-scheme-parsing")
    {
      parse_protect_global = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "check-internal-types")
    {
      do_internal_type_checking_global = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "debug-gc-assert-parsed-dead")
    {
      parsed_objects_should_be_dead = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "safe")
    {
      be_safe_global = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "old-relative")
    {
      lily_1_8_relative = valbool;
      /* Needs to be reset for each file that uses this option. */
      lily_1_8_compatibility_used = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "strict-infinity-checking")
    {
      strict_infinity_checking = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "debug-skylines")
    {
      debug_skylines = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "debug-property-callbacks")
    {
      debug_property_callbacks = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "debug-page-breaking-scoring")
    {
      debug_page_breaking_scoring = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "datadir")
    {
      /* ignore input value. */
      val = ly_string2scm (lilypond_datadir);
    }
  else if (varstr == "relative-includes")
    {
      relative_includes = valbool;
      val = val_scm_bool;
    }
  else if (varstr == "warning-as-error")
    val = val_scm_bool;
  else if (varstr == "music-strings-to-paths")
    {
      music_strings_to_paths = valbool;
      val = val_scm_bool;
    }

  scm_hashq_set_x (option_hash, var, val);
}

ssize const HELP_INDENT = 30;
ssize const INDENT = 2;
ssize const SEPARATION = 5;

/*
  Hmmm. should do in SCM / C++  ?
*/
static string
get_help_string ()
{
  SCM alist = ly_hash2alist (option_hash);
  SCM converter = ly_lily_module_constant ("scm->string");

  vector<string> opts;

  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM sym = scm_caar (s);
      SCM val = scm_cdar (s);
      string opt_spec = String_convert::char_string (' ', INDENT)
                        + ly_symbol2string (sym)
                        + " ("
                        + ly_scm2string (scm_call_1 (converter, val))
                        + ")";

      if (opt_spec.length () + SEPARATION > HELP_INDENT)
        opt_spec += "\n" + String_convert::char_string (' ', HELP_INDENT);
      else
        opt_spec += String_convert::char_string (' ', HELP_INDENT
                                                 - opt_spec.length ());

      SCM opt_help_scm
        = scm_object_property (sym,
                               ly_symbol2scm ("program-option-documentation"));
      string opt_help = ly_scm2string (opt_help_scm);
      replace_all (&opt_help,
                   string ("\n"),
                   string ("\n")
                   + String_convert::char_string (' ', HELP_INDENT));

      opts.push_back (opt_spec + opt_help + "\n");
    }

  string help ("Options supported by `ly:set-option':\n\n");
  vector_sort (opts, less<string> ());
  for (vsize i = 0; i < opts.size (); i++)
    help += opts[i];
  return help;
}

LY_DEFINE (ly_option_usage, "ly:option-usage", 0, 0, 0, (),
           "Print @code{ly:set-option} usage.")
{
  string help = get_help_string ();
  puts (help.c_str ());
  fflush (stdout);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_add_option, "ly:add-option", 3, 0, 0,
           (SCM sym, SCM val, SCM description),
           "Add a program option @var{sym}.  @var{val} is the default"
           " value and @var{description} is a string description.")
{
  if (!option_hash)
    option_hash = scm_permanent_object (scm_c_make_hash_table (11));
  LY_ASSERT_TYPE (ly_is_symbol, sym, 1);
  LY_ASSERT_TYPE (scm_is_string, description, 3);

  internal_set_option (sym, val);

  scm_set_object_property_x (sym, ly_symbol2scm ("program-option-documentation"),
                             description);

  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_set_option, "ly:set-option", 1, 1, 0, (SCM var, SCM val),
           "Set a program option.")
{
  LY_ASSERT_TYPE (ly_is_symbol, var, 1);

  if (val == SCM_UNDEFINED)
    val = SCM_BOOL_T;

  string varstr = robust_symbol2string (var, "");
  if (varstr.substr (0, 3) == string ("no-"))
    {
      var = ly_symbol2scm (varstr.substr (3, varstr.length () - 3).c_str ());
      val = scm_from_bool (!to_boolean (val));
    }

  SCM handle = scm_hashq_get_handle (option_hash, var);
  if (handle == SCM_BOOL_F)
    warning (_f ("no such internal option: %s", varstr.c_str ()));

  internal_set_option (var, val);
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_command_line_options, "ly:command-line-options", 0, 0, 0, (),
           "The Scheme options specified on command-line with @option{-d}.")
{
  return ly_string2scm (init_scheme_variables_global);
}

LY_DEFINE (ly_command_line_code, "ly:command-line-code", 0, 0, 0, (),
           "The Scheme code specified on command-line with @option{-e}.")
{
  return ly_string2scm (init_scheme_code_global);
}

LY_DEFINE (ly_verbose_output_p, "ly:verbose-output?", 0, 0, 0, (),
           "Was verbose output requested, i.e. loglevel at least @code{DEBUG}?")
{
  return scm_from_bool (is_loglevel (LOG_DEBUG));
}

LY_DEFINE (ly_all_options, "ly:all-options",
           0, 0, 0, (),
           "Get all option settings in an alist.")
{
  return ly_hash2alist (option_hash);
}

LY_DEFINE (ly_get_option, "ly:get-option", 1, 0, 0, (SCM var),
           "Get a global option setting.")
{
  LY_ASSERT_TYPE (ly_is_symbol, var, 1);
  return scm_hashq_ref (option_hash, var, SCM_BOOL_F);
}
