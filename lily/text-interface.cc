/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "text-interface.hh"
#include "skyline-pair.hh"

#include "lookup.hh"
#include "config.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "main.hh"
#include "misc.hh"
#include "modified-font-metric.hh"
#include "output-def.hh"
#include "pango-font.hh"
#include "program-option.hh"
#include "international.hh"
#include "warn.hh"
#include "lily-imports.hh"

#include <map>

using std::string;

// This is a little bit ugly, but the replacement alist is setup in
// the layout block so it'll be the same across invocations.

std::map<string, string> replacement_cache;
vsize replacement_max;
Protected_scm replacement_cache_alist_key;

static void
populate_cache (SCM alist)
{
  if (scm_is_eq (alist, static_cast<SCM> (replacement_cache_alist_key)))
    return;

  replacement_cache.clear ();
  replacement_max = 0;
  replacement_cache_alist_key = alist;
  for (SCM h = alist; scm_is_pair (h); h = scm_cdr (h))
    {
      SCM k = scm_caar (h);
      SCM v = scm_cdar (h);
      if (!scm_is_string (k) || !scm_is_string (v))
        continue;

      string orig = ly_scm2string (k);
      if (orig.empty ())
        continue;

      string dest = ly_scm2string (v);

      // If an alist has duplicate key entries, only the first must be
      // considered.
      if (replacement_cache.insert ({orig, dest}).second)
        replacement_max = std::max (replacement_max, orig.size ());
    }
}

static void
replace_special_characters (string &str, SCM props)
{
  for (vsize i = 0; i < str.size (); i++)
    if (isspace (str[i]))
      str[i] = ' ';

  SCM replacement_alist = ly_chain_assoc_get (ly_symbol2scm ("replacement-alist"),
                                              props,
                                              SCM_EOL);

  if (replacement_alist == SCM_EOL || str.empty ())
    return;

  populate_cache (replacement_alist);

  for (vsize i = 0; i < str.size (); i++)
    {
      do
        {
          // replacement loop while consecutive replacements happen
          // We break out when this is not the case

          /* Don't match in mid-UTF-8 */
          if ((str[i] & 0xc0) == 0x80)
            break;

          // Using a C++17 string_view here instead of substr would
          // obviate the need for maintaining replacement_max in order
          // to keep complexity from becoming quadratic in string
          // length.
          auto it = replacement_cache.upper_bound (str.substr (i, replacement_max));
          if (it == replacement_cache.begin ())
            break;

          --it;
          // The iterator it now points to largest key smaller or equal
          // to string tail.  In the case of several keys matching for
          // their respective entire length, the longest matching key
          // will be taken, being the lexicographically largest one.

          const string &key = it->first;
          vsize j = key.length ();
          // Make sure that entire key is present in the source string
          if (str.compare (i, j, key) != 0)
            break;

          const string &replacement = it->second;
          str.replace (i, j, replacement);
          // Stop doing replacements at this position and advance in the
          // string up to just after the text we replaced with. This
          // ensures that the result of a replacement is never processed
          // itself for replacements.
          i += replacement.length ();
        }
      while (i < str.size ());
    }
}

MAKE_SCHEME_CALLBACK (Text_interface, interpret_string, 3);
SCM
Text_interface::interpret_string (SCM layout_smob,
                                  SCM props,
                                  SCM markup)
{
  LY_ASSERT_SMOB (Output_def, layout_smob, 1);
  LY_ASSERT_TYPE (scm_is_string, markup, 3);

  string str = ly_scm2string (markup);
  Output_def *layout = unsmob<Output_def> (layout_smob);
  Font_metric *fm = select_encoded_font (layout, props);

  replace_special_characters (str, props);

  /*
    We want to filter strings with a music font that pass through
    the text interface.  Here the font encoding is checked to see
    if it matches one of the music font encodings.  --pmccarty
  */
  SCM encoding = ly_chain_assoc_get (ly_symbol2scm ("font-encoding"),
                                     props,
                                     SCM_BOOL_F);
  SCM music_encodings = Lily::all_music_font_encodings;

  SCM features = ly_chain_assoc_get (ly_symbol2scm ("font-features"),
                                     props,
                                     SCM_BOOL_F);

  // The font-features value is stored in a scheme list. This joins the entries
  // with commas for processing with pango.
  string features_str = string ();
  if (scm_is_pair (features))
    {
      bool first = true;
      for (SCM s = features; scm_is_pair (s); s = scm_cdr (s))
        {
          SCM feature = scm_car (s);
          if (scm_is_string (feature))
            {
              if (first)
                {
                  first = false;
                }
              else
                {
                  features_str += ",";
                }
              features_str += ly_scm2string (feature);
            }
          else
            {
              scm_misc_error (__FUNCTION__, "Found non-string in font-features list", SCM_EOL);
            }
        }
    }
  else if (!scm_is_false (features))
    {
      scm_misc_error (__FUNCTION__, "Expecting a list for font-features value", SCM_EOL);
    }

  bool is_music = scm_is_true (scm_memq (encoding, music_encodings));
  return fm->text_stencil (layout, str, is_music, features_str).smobbed_copy ();
}

static size_t markup_depth = 0;

void markup_up_depth (void *) { ++markup_depth; }
void markup_down_depth (void *) { --markup_depth; }

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Text_interface, interpret_markup, 3, 0,
                                   "Convert a text markup into a stencil."
                                   "  Takes three arguments, @var{layout}, @var{props}, and @var{markup}.\n"
                                   "\n"
                                   "@var{layout} is a @code{\\layout} block; it may be obtained from a grob with"
                                   " @code{ly:grob-layout}.  @var{props} is an alist chain, i.e. a list of"
                                   "  alists.  This is typically obtained with"
                                   " @code{(ly:grob-alist-chain grob (ly:output-def-lookup layout 'text-font-defaults))}."
                                   "  @var{markup} is the markup text to be processed.");
SCM
Text_interface::interpret_markup (SCM layout_smob, SCM props, SCM markup)
{
  auto *layout = LY_ASSERT_SMOB (Output_def, layout_smob, 1);

  return internal_interpret_markup (layout, props, markup);
}

Stencil
Text_interface::interpret_markup (Output_def *layout, SCM props, SCM markup)
{
  Stencil result;
  SCM st_scm = internal_interpret_markup (layout, props, markup);
  if (auto *st = unsmob<const Stencil> (st_scm))
    result = *st;
  return result;
}

SCM
Text_interface::internal_interpret_markup (Output_def *layout,
                                           SCM props, SCM markup)
{
  if (scm_is_string (markup))
    {
      /*
        Setting markup variables to an empty string "" is commonly used
        in paper and layout properties; such variables must be treated
        exactly like #f, and return an empty stencil. -vv
      */
      if (scm_is_true (scm_string_null_p (markup)))
        return Stencil ().smobbed_copy ();
      else
        return interpret_string (to_scm (layout), props, markup);
    }
  else if (is_markup (markup))
    {
      SCM func = scm_car (markup);
      SCM args = scm_cdr (markup);

      /* Check for non-terminating markups, e.g. recursive calls with
       * changing arguments */
      SCM opt_depth = ly_get_option (ly_symbol2scm ("max-markup-depth"));
      size_t max_depth = from_scm (opt_depth, 1024);

      // Don't use SCM_F_DYNWIND_REWINDABLE since it may be expensive
      // without any obvious use for retaining continuations into
      // markup expansion
      scm_dynwind_begin ((scm_t_dynwind_flags)0);
      // scm_dynwind_rewind_handler (markup_up_depth, 0, SCM_F_WIND_EXPLICITLY);
      markup_up_depth (0);
      scm_dynwind_unwind_handler (markup_down_depth, 0, SCM_F_WIND_EXPLICITLY);
      if (markup_depth > max_depth)
        {
          scm_dynwind_end ();
          string name = ly_symbol2string (scm_procedure_name (func));
          // TODO: Also print the arguments of the markup!
          non_fatal_error (_f ("Markup depth exceeds maximal value of %zu; "
                               "Markup: %s", max_depth, name.c_str ()));
          return Stencil ().smobbed_copy ();
        }

      SCM retval = scm_apply_2 (func, to_scm (layout), props, args);
      scm_dynwind_end ();
      return retval;
    }
  else
    {
      programming_error ("Object is not a markup.");
      scm_puts ("This object should be a markup: ", scm_current_error_port ());
      scm_display (markup, scm_current_error_port ());
      scm_puts ("\n", scm_current_error_port ());

      return Stencil ().smobbed_copy ();
    }
}

MAKE_SCHEME_CALLBACK (Text_interface, print, 1);
SCM
Text_interface::print (SCM grob)
{
  auto *me = LY_ASSERT_SMOB (Grob, grob, 1);

  return internal_print (me);
}

Stencil
Text_interface::print (Grob *me)
{
  Stencil result;
  SCM st_scm = internal_print (me);
  if (auto *st = unsmob<const Stencil> (st_scm))
    result = *st;
  return result;
}

SCM
Text_interface::internal_print (Grob *me)
{
  SCM t = get_property (me, "text");
  SCM chain = Font_interface::text_font_alist_chain (me);
  return interpret_markup (me->layout ()->self_scm (), chain, t);
}

/* Ugh. Duplicated from Scheme.  */
bool
Text_interface::is_markup (SCM x)
{
  return scm_is_string (x)
         || (scm_is_pair (x)
             && scm_is_true (Lily::markup_command_signature (scm_car (x)))
             && scm_is_false (Lily::markup_list_function_p (scm_car (x))));
}
bool
Text_interface::is_markup_list (SCM x)
{
  return scm_is_true (Lily::markup_list_p (x));
}

ADD_INTERFACE (Text_interface,
               "A Scheme markup text, see @ruser{Formatting text} and"
               " @rextend{New markup command definition}.\n"
               "\n"
               "There are two important commands:"
               " @code{ly:text-interface::print}, which is a"
               " grob callback, and"
               " @code{ly:text-interface::interpret-markup}.",

               /* properties */
               "baseline-skip "
               "replacement-alist "
               "text "
               "word-space "
               "text-direction "
               "flag-style "
              );
